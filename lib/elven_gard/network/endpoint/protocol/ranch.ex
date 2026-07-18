defmodule ElvenGard.Network.Endpoint.Protocol.Ranch do
  @moduledoc false

  use GenServer

  alias ElvenGard.Network.PacketProcessor
  alias ElvenGard.Network.Socket
  alias ElvenGard.Network.Socket.Adapters.Ranch, as: RanchAdapter

  @behaviour :ranch_protocol

  defmodule State do
    @moduledoc false

    alias ElvenGard.Network.Socket

    @enforce_keys [:socket, :socket_handler, :packet_handler]
    defstruct [:socket, :socket_handler, :packet_handler]

    @type t :: %__MODULE__{
            socket: Socket.t(),
            socket_handler: module(),
            packet_handler: module()
          }
  end

  ## Ranch callbacks

  @impl :ranch_protocol
  def start_link(ref, transport, opts) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
  end

  ## GenServer callbacks

  @impl GenServer
  def init({ref, transport, opts}) do
    socket_handler = Keyword.fetch!(opts, :socket_handler)
    otp_app = Keyword.fetch!(opts, :otp_app)
    handler_config = Application.fetch_env!(otp_app, socket_handler)
    codec = Keyword.fetch!(handler_config, :network_codec)
    packet_handler = Keyword.fetch!(handler_config, :packet_handler)

    {:ok, transport_socket} = :ranch.handshake(ref)

    socket =
      Socket.new(
        RanchAdapter,
        [transport: transport, socket: transport_socket],
        codec
      )

    state = %State{
      socket: socket,
      socket_handler: socket_handler,
      packet_handler: packet_handler
    }

    init_error =
      "handle_init/1 must return `{:ok, socket}`, `{:ok, socket, timeout}` " <>
        "or `{:stop, reason, new_socket}`"

    case socket_handler.handle_init(socket) do
      {:ok, %Socket{} = new_socket} ->
        do_enter_loop(%State{state | socket: new_socket})

      {:ok, %Socket{} = new_socket, timeout} ->
        do_enter_loop(%State{state | socket: new_socket}, timeout)

      {:stop, reason, %Socket{} = new_socket} ->
        {:stop, reason, %State{state | socket: new_socket}}

      _ ->
        raise init_error
    end
  end

  @impl GenServer
  def handle_info({transport, _transport_socket, data}, %State{} = state)
      when transport in [:tcp, :ssl] do
    %State{socket: socket, socket_handler: socket_handler} = state
    %Socket{remaining: remaining} = socket

    full_data =
      case remaining do
        <<>> -> data
        _ -> :erlang.iolist_to_binary([remaining | data])
      end

    result =
      case socket_handler.handle_message(full_data, socket) do
        :ignore ->
          {:noreply, %State{state | socket: %Socket{socket | remaining: <<>>}}}

        {:ignore, %Socket{} = new_socket} ->
          {:noreply, %State{state | socket: %Socket{new_socket | remaining: <<>>}}}

        {:ok, %Socket{} = new_socket} ->
          process_packets(full_data, new_socket, state)

        {:stop, reason, %Socket{} = new_socket} ->
          {:stop, reason, %State{state | socket: new_socket}}

        value ->
          raise "invalid return value for handle_message/2 (got: #{inspect(value)})"
      end

    rearm_transport(result)
    result
  end

  @impl GenServer
  def handle_info({transport, _transport_socket}, %State{} = state)
      when transport in [:tcp_closed, :ssl_closed] do
    %State{socket: socket} = state
    do_handle_halt(:closed, socket, state)
  end

  @impl GenServer
  def handle_info({transport, _transport_socket, reason}, %State{} = state)
      when transport in [:tcp_error, :ssl_error] do
    %State{socket: socket} = state
    do_handle_halt({:error, reason}, socket, state)
  end

  @impl GenServer
  def handle_info(:timeout, %State{} = state) do
    %State{socket: socket} = state
    do_handle_halt(:timeout, socket, state)
  end

  ## Private function

  defp do_enter_loop(%State{} = state, timeout \\ :infinity) do
    %State{socket: socket} = state
    Socket.setopts(socket, active: :once)
    :gen_server.enter_loop(__MODULE__, [], state, timeout)
  end

  defp rearm_transport(result) do
    case result do
      {:noreply, %State{} = state} ->
        %State{socket: socket} = state
        Socket.setopts(socket, active: :once)

      _ ->
        :ok
    end
  end

  defp process_packets(data, %Socket{} = socket, %State{} = state) do
    %State{packet_handler: packet_handler} = state
    %Socket{encoder: codec} = socket

    case PacketProcessor.process(data, socket, codec, packet_handler) do
      {:cont, new_socket} -> {:noreply, %State{state | socket: new_socket}}
      {:halt, reason, new_socket} -> do_handle_halt(reason, new_socket, state)
    end
  end

  defp do_handle_halt(reason, socket, %State{} = state) do
    %State{socket_handler: socket_handler} = state
    Socket.close(socket)

    case socket_handler.handle_halt(reason, socket) do
      {:ok, %Socket{} = new_socket} ->
        {:stop, :normal, %State{state | socket: new_socket}}

      {:ok, stop_reason, %Socket{} = new_socket} ->
        {:stop, stop_reason, %State{state | socket: new_socket}}

      _ ->
        raise "handle_halt/2 must return `{:ok, socket}` or `{:ok, stop_reason, socket}`"
    end
  end
end
