defmodule ElvenGard.Frontend.State do
  @moduledoc false

  require Record

  alias ElvenGard.Socket

  Record.defrecord(:frontend_state,
    callback_module: nil,
    packet_handler: nil,
    packet_protocol: nil,
    socket: nil
  )

  @type t ::
          record(:frontend_state,
            callback_module: module,
            packet_handler: module,
            packet_protocol: module,
            socket: Socket.t()
          )
end

defmodule ElvenGard.Frontend.RanchProtocol do
  @moduledoc false

  @behaviour :ranch_protocol

  use GenServer

  import ElvenGard.Frontend.State

  alias ElvenGard.Frontend
  alias ElvenGard.Socket

  @timeout 5_000

  ## Ranch behaviour

  @impl :ranch_protocol
  def start_link(ref, socket, transport, protocol_options) do
    opts = {
      ref,
      socket,
      transport,
      protocol_options
    }

    pid = :proc_lib.spawn_link(__MODULE__, :init, [opts])
    {:ok, pid}
  end

  ## Genserver behaviour

  @impl GenServer
  @spec init({reference, pid, atom, keyword}) :: no_return
  def init({ref, socket, transport, protocol_options}) do
    callback_module = Keyword.fetch!(protocol_options, :callback_module)
    packet_protocol = Keyword.fetch!(protocol_options, :packet_protocol)
    packet_handler = Keyword.fetch!(protocol_options, :packet_handler)

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, active: true, nodelay: true)

    {:ok, socket} =
      socket
      |> Socket.new(transport, packet_protocol)
      |> callback_module.handle_connection()

    state =
      frontend_state(
        callback_module: callback_module,
        packet_protocol: packet_protocol,
        packet_handler: packet_handler,
        socket: socket
      )

    :gen_server.enter_loop(__MODULE__, [], state, @timeout)
  end

  @impl GenServer
  def handle_info({:tcp, _socket, data}, state) do
    frontend_state(
      callback_module: cb_module,
      packet_handler: handler,
      packet_protocol: protocol,
      socket: socket
    ) = state

    # TODO: Manage errors on `handle_message`: don't execute the protocol ???
    {:ok, tmp_socket} = cb_module.handle_message(socket, data)

    payload = protocol.decode(data, tmp_socket)

    case do_handle_packet(payload, tmp_socket, handler) do
      {:cont, final_socket} ->
        {:noreply, frontend_state(state, socket: final_socket)}

      {:halt, {:ok, args}, final_socket} ->
        new_state = frontend_state(state, socket: final_socket)
        do_halt_ok(args, new_state)

      {:halt, {:error, reason}, final_socket} ->
        new_state = frontend_state(state, socket: final_socket)
        do_halt_error(reason, new_state)

      x ->
        raise """
        #{inspect(handler)}.handle_packet/3 have to return `{:cont, socket}`, \
        `{:halt, {:ok, :some_args}, socket}`, or `{:halt, {:error, reason}, socket} `. \
        Returned: #{inspect(x)}
        """
    end
  end

  @impl GenServer
  def handle_info({:tcp_closed, _socket}, state) do
    frontend_state(callback_module: cb_module, socket: socket) = state
    {:ok, new_socket} = cb_module.handle_disconnection(socket, :normal)
    {:stop, :normal, frontend_state(state, socket: new_socket)}
  end

  @impl GenServer
  def handle_info({:tcp_error, _socket, reason}, state) do
    frontend_state(callback_module: cb_module, socket: socket) = state
    {:ok, new_socket} = cb_module.handle_error(socket, reason)
    {:stop, reason, frontend_state(state, socket: new_socket)}
  end

  @impl GenServer
  def handle_info(:timeout, state) do
    frontend_state(callback_module: cb_module, socket: socket) = state
    {:ok, new_socket} = cb_module.handle_error(socket, :timeout)
    {:stop, :normal, frontend_state(state, socket: new_socket)}
  end

  ## Private function

  @doc false
  @spec do_handle_packet({:error, term} | {term, map} | [{term, map}, ...], Socket.t(), module) ::
          {:cont, Socket.t()}
          | {:halt, {:ok, term}, Socket.t()}
          | {:halt, {:error, Frontend.conn_error()}, Socket.t()}
  defp do_handle_packet({:error, reason}, socket, _handler) do
    {:halt, {:error, reason}, socket}
  end

  defp do_handle_packet({header, params}, socket, handler) do
    handler.handle_packet(header, params, socket)
  end

  defp do_handle_packet([{_header, _params} | _t] = packet_list, socket, handler) do
    Enum.reduce_while(packet_list, {:cont, socket}, fn packet, {_, socket} ->
      res = do_handle_packet(packet, socket, handler)
      {elem(res, 0), res}
    end)
  end

  defp do_handle_packet(x, _socket, _handler) do
    raise """
    unable to handle packet #{inspect(x)}.
    Please check that your protocol returns a tuple in the form of {header, \
    %{param1: :val1, param2: :val2, ...}, a list of this tuple or {:error, reason}
    """
  end

  @doc false
  @spec do_halt_ok(term, State.t()) :: {:stop, :normal, State.t()}
  defp do_halt_ok(args, state) do
    frontend_state(callback_module: cb_module, socket: socket) = state
    new_socket = socket |> cb_module.handle_halt_ok(args) |> close_socket(:normal, cb_module)
    new_state = frontend_state(state, socket: new_socket)
    {:stop, :normal, new_state}
  end

  @doc false
  @spec do_halt_error(term, State.t()) :: {:stop, :normal, State.t()}
  defp do_halt_error(reason, state) do
    frontend_state(callback_module: cb_module, socket: socket) = state
    new_socket = socket |> cb_module.handle_halt_error(reason) |> close_socket(reason, cb_module)
    new_state = frontend_state(state, socket: new_socket)
    {:stop, :normal, new_state}
  end

  @doc false
  @spec close_socket(ElvenGard.Frontend.handle_return(), term, module) :: Socket.t()
  defp close_socket({:ok, %Socket{} = socket}, reason, cb_module),
    do: do_close_socket(socket, reason, cb_module)

  defp close_socket({:error, _, %Socket{} = socket}, reason, cb_module),
    do: do_close_socket(socket, reason, cb_module)

  @doc false
  @spec do_close_socket(Socket.t(), term, module) :: Socket.t()
  defp do_close_socket(%Socket{} = socket, reason, cb_module) do
    %Socket{transport_pid: transport_pid, transport: transport} = socket
    {:ok, final_socket} = cb_module.handle_disconnection(socket, reason)
    transport.close(transport_pid)
    final_socket
  end
end

defmodule ElvenGard.Frontend do
  @moduledoc """
  TODO: Documentation for ElvenGard.Frontend
  """

  alias ElvenGard.Socket

  @type conn_error :: atom | binary | bitstring
  @type handle_ok :: {:ok, Socket.t()}
  @type handle_error :: {:error, term, Socket.t()}
  @type handle_return :: handle_ok | handle_error

  @callback handle_init(args :: map) ::
              {:ok, map}
              | {:ok, map, timeout() | :hibernate | {:continue, term()}}
              | :ignore
              | {:stop, reason :: any()}
  @callback handle_connection(socket :: Socket.t()) :: handle_return
  @callback handle_disconnection(socket :: Socket.t(), reason :: term) :: handle_return
  @callback handle_message(socket :: Socket.t(), message :: binary) :: handle_return
  @callback handle_error(socket :: Socket.t(), error :: conn_error) :: handle_return
  @callback handle_halt_ok(socket :: Socket.t(), args :: term) :: handle_return
  @callback handle_halt_error(socket :: Socket.t(), error :: conn_error) :: handle_return

  @callback send(socket :: Socket.t(), message :: any()) :: :ok | {:error, atom()}

  @doc false
  defmacro __using__(opts) do
    caller = __CALLER__.module
    port = Keyword.get(opts, :port, 3000)
    protocol = Keyword.get(opts, :packet_protocol)
    handler = Keyword.get(opts, :packet_handler)

    # Check if there is any protocol
    unless protocol do
      raise "please, specify a packet_protocol for #{caller}"
    end

    # Check if there is any handler
    unless handler do
      raise "please, specify a packet_handler for #{caller}"
    end

    quote do
      @behaviour unquote(__MODULE__)

      use GenServer

      def start_link(args) do
        GenServer.start_link(__MODULE__, args, name: __MODULE__)
      end

      @impl GenServer
      def init(_) do
        ip_address = Application.get_env(:elven_gard, :ip_address, "127.0.0.1")
        port = unquote(port)
        {:ok, ranch_ip} = ip_address |> to_charlist() |> :inet.parse_ipv4_address()

        listener_opts = %{
          num_acceptors: Application.get_env(:elven_gard, :num_acceptors, 10),
          max_connections: Application.get_env(:elven_gard, :max_connections, 1024),
          handshake_timeout: Application.get_env(:elven_gard, :handshake_timeout, 5000),
          socket_opts: [ip: ranch_ip, port: port]
        }

        opts = %{
          listener_name: {__MODULE__, ip_address, port},
          transport: :ranch_tcp,
          listener_opts: listener_opts,
          protocol: unquote(__MODULE__).RanchProtocol,
          protocol_opts: [
            callback_module: __MODULE__,
            packet_protocol: unquote(protocol),
            packet_handler: unquote(handler)
          ]
        }

        case handle_init(opts) do
          {:ok, opts} -> {:ok, nil, {:continue, {:init_listener, opts, nil}}}
          {:ok, opts, action} -> {:ok, nil, {:continue, {:init_listener, opts, action}}}
          res -> res
        end
      end

      @impl GenServer
      def handle_continue({:init_listener, opts, action}, state) do
        %{
          listener_name: listener_name,
          transport: transport,
          listener_opts: listener_opts,
          protocol: protocol,
          protocol_opts: protocol_opts
        } = opts

        {:ok, pid} =
          :ranch.start_listener(
            listener_name,
            transport,
            listener_opts,
            protocol,
            protocol_opts
          )

        # FIXMME: Not sure if it's a good practice....
        # Maybe a monitor would be better
        Process.link(pid)

        case action do
          nil -> {:noreply, state}
          _ -> {:noreply, state, action}
        end
      end

      #
      # Default implementations
      #

      def handle_init(_args), do: {:ok, nil}
      def handle_connection(socket), do: {:ok, socket}
      def handle_disconnection(socket, _reason), do: {:ok, socket}
      def handle_message(socket, _message), do: {:ok, socket}
      def handle_error(socket, _reason), do: {:ok, socket}
      def handle_halt_ok(socket, _args), do: {:ok, socket}
      def handle_halt_error(socket, _reason), do: {:ok, socket}

      defoverridable handle_init: 1,
                     handle_connection: 1,
                     handle_disconnection: 2,
                     handle_message: 2,
                     handle_error: 2,
                     handle_halt_ok: 2,
                     handle_halt_error: 2
    end
  end
end
