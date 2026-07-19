if Code.ensure_loaded?(:ranch) do
  defmodule ElvenGard.Network.Endpoint.Protocol.Ranch do
    @moduledoc false

    use GenServer

    alias ElvenGard.Network.Endpoint.Connection
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
      codec = Keyword.fetch!(opts, :network_codec)
      packet_handler = Keyword.fetch!(opts, :packet_handler)

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

      case Connection.init(socket, socket_handler) do
        {:cont, new_socket} ->
          do_enter_loop(%State{state | socket: new_socket})

        {:cont, new_socket, timeout} ->
          do_enter_loop(%State{state | socket: new_socket}, timeout)

        {:halt, reason, new_socket} ->
          do_handle_halt(reason, new_socket, state)
      end
    end

    @impl GenServer
    def handle_info({transport, _transport_socket, data}, %State{} = state)
        when transport in [:tcp, :ssl] do
      %State{
        socket: socket,
        socket_handler: socket_handler,
        packet_handler: packet_handler
      } = state

      result =
        case Connection.process(data, socket, socket_handler, packet_handler) do
          {:cont, new_socket} -> {:noreply, %State{state | socket: new_socket}}
          {:halt, reason, new_socket} -> do_handle_halt(reason, new_socket, state)
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

    def handle_info(message, %State{} = state) do
      %State{socket: socket, socket_handler: socket_handler} = state

      case Connection.info(message, socket, socket_handler) do
        {:cont, new_socket} -> {:noreply, %State{state | socket: new_socket}}
        {:halt, reason, new_socket} -> do_handle_halt(reason, new_socket, state)
      end
    end

    @impl GenServer
    def terminate(reason, %State{} = state) do
      case reason do
        :normal ->
          :ok

        :shutdown ->
          halt_after_termination(:closed, state)

        {:shutdown, _reason} ->
          halt_after_termination(:closed, state)

        error ->
          halt_after_termination({:error, error}, state)
      end
    end

    ## Private function

    defp do_enter_loop(%State{} = state, timeout \\ :infinity) do
      %State{socket: socket} = state
      Process.flag(:trap_exit, true)
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

    defp do_handle_halt(reason, socket, %State{} = state) do
      %State{socket_handler: socket_handler} = state
      new_socket = halt_connection(reason, socket, socket_handler)

      {:stop, :normal, %State{state | socket: new_socket}}
    end

    defp halt_after_termination(reason, %State{} = state) do
      %State{socket: socket, socket_handler: socket_handler} = state
      _socket = halt_connection(reason, socket, socket_handler)
      :ok
    end

    defp halt_connection(reason, socket, socket_handler) do
      Socket.close(socket)
      Connection.halt(reason, socket, socket_handler)
    end
  end
end
