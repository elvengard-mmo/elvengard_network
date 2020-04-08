defmodule ElvenGard.Frontend.State do
  @moduledoc false

  require Record

  alias ElvenGard.Structures.Client

  Record.defrecord(:frontend_state,
    callback_module: nil,
    packet_handler: nil,
    packet_protocol: nil,
    client: nil
  )

  @type t ::
          record(:frontend_state,
            callback_module: module,
            packet_handler: module,
            packet_protocol: module,
            client: Client.t()
          )
end

defmodule ElvenGard.Frontend.RanchProtocol do
  @moduledoc false

  @behaviour :ranch_protocol

  use GenServer

  import ElvenGard.Frontend.State

  alias ElvenGard.Frontend
  alias ElvenGard.Structures.Client

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
    :ok = transport.setopts(socket, [{:active, true}])

    {:ok, client} =
      socket
      |> Client.new(transport, packet_protocol)
      |> callback_module.handle_connection()

    state =
      frontend_state(
        callback_module: callback_module,
        packet_protocol: packet_protocol,
        packet_handler: packet_handler,
        client: client
      )

    :gen_server.enter_loop(__MODULE__, [], state, @timeout)
  end

  @impl GenServer
  def handle_info({:tcp, _socket, data}, state) do
    frontend_state(
      callback_module: cb_module,
      packet_handler: handler,
      packet_protocol: protocol,
      client: client
    ) = state

    # TODO: Manage errors on `handle_message`: don't execute the protocol ???
    {:ok, tmp_client} = cb_module.handle_message(client, data)

    payload = protocol.complete_decode(data, tmp_client)

    case do_handle_packet(payload, tmp_client, handler) do
      {:cont, final_client} ->
        {:noreply, frontend_state(state, client: final_client)}

      {:halt, {:ok, args}, final_client} ->
        new_state = frontend_state(state, client: final_client)
        do_halt_ok(args, new_state)

      {:halt, {:error, reason}, final_client} ->
        new_state = frontend_state(state, client: final_client)
        do_halt_error(reason, new_state)

      x ->
        raise """
        #{inspect(handler)}.handle_packet/3 have to return `{:cont, client}`, \
        `{:halt, {:ok, :some_args}, client}`, or `{:halt, {:error, reason}, client} `. \
        Returned: #{inspect(x)}
        """
    end
  end

  @impl GenServer
  def handle_info({:tcp_closed, _socket}, state) do
    frontend_state(callback_module: cb_module, client: client) = state
    {:ok, new_client} = cb_module.handle_disconnection(client, :normal)
    {:stop, :normal, frontend_state(state, client: new_client)}
  end

  @impl GenServer
  def handle_info({:tcp_error, _socket, reason}, state) do
    frontend_state(callback_module: cb_module, client: client) = state
    {:ok, new_client} = cb_module.handle_error(client, reason)
    {:stop, reason, frontend_state(state, client: new_client)}
  end

  @impl GenServer
  def handle_info(:timeout, state) do
    frontend_state(callback_module: cb_module, client: client) = state
    {:ok, new_client} = cb_module.handle_error(client, :timeout)
    {:stop, :normal, frontend_state(state, client: new_client)}
  end

  ## Private function

  @doc false
  @spec do_handle_packet({term, map} | list(tuple), Client.t(), module) ::
          {:cont, Client.t()}
          | {:halt, {:ok, term}, Client.t()}
          | {:halt, {:error, Frontend.conn_error()}, Client.t()}
  defp do_handle_packet({header, params}, client, handler) do
    handler.handle_packet(header, params, client)
  end

  defp do_handle_packet([{_header, _params} | _t] = packet_list, client, handler) do
    Enum.reduce_while(packet_list, {:cont, client}, fn packet, {_, client} ->
      res = do_handle_packet(packet, client, handler)
      {elem(res, 0), res}
    end)
  end

  defp do_handle_packet(x, _client, _handler) do
    raise """
    unable to handle packet #{inspect(x)}.
    Please check that your protocol returns a tuple in the form of {header, \
    %{param1: :val1, param2: :val2, ...} or a list of tuples
    """
  end

  @doc false
  @spec do_halt_ok(term, State.t()) :: {:stop, :normal, State.t()}
  defp do_halt_ok(args, state) do
    frontend_state(callback_module: cb_module, client: client) = state
    new_client = client |> cb_module.handle_halt_ok(args) |> close_socket(:normal, cb_module)
    new_state = frontend_state(state, client: new_client)
    {:stop, :normal, new_state}
  end

  @doc false
  @spec do_halt_error(term, State.t()) :: {:stop, :normal, State.t()}
  defp do_halt_error(reason, state) do
    frontend_state(callback_module: cb_module, client: client) = state
    new_client = client |> cb_module.handle_halt_error(reason) |> close_socket(reason, cb_module)
    new_state = frontend_state(state, client: new_client)
    {:stop, :normal, new_state}
  end

  @doc false
  @spec close_socket(ElvenGard.Frontend.handle_return(), term, module) :: Client.t()
  defp close_socket({:ok, %Client{} = client}, reason, cb_module),
    do: do_close_socket(client, reason, cb_module)

  defp close_socket({:error, _, %Client{} = client}, reason, cb_module),
    do: do_close_socket(client, reason, cb_module)

  @doc false
  @spec do_close_socket(Client.t(), term, module) :: Client.t()
  defp do_close_socket(%Client{} = client, reason, cb_module) do
    %Client{socket: socket, transport: transport} = client
    {:ok, final_client} = cb_module.handle_disconnection(client, reason)
    transport.close(socket)
    final_client
  end
end

defmodule ElvenGard.Frontend do
  @moduledoc """
  TODO: Documentation for ElvenGard.Frontend
  """

  alias ElvenGard.Structures.Client

  @type conn_error :: atom | binary | bitstring
  @type handle_ok :: {:ok, Client.t()}
  @type handle_error :: {:error, term, Client.t()}
  @type handle_return :: handle_ok | handle_error

  @callback handle_init(args :: map) ::
              {:ok, map}
              | {:ok, map, timeout() | :hibernate | {:continue, term()}}
              | :ignore
              | {:stop, reason :: any()}
  @callback handle_connection(client :: Client.t()) :: handle_return
  @callback handle_disconnection(client :: Client.t(), reason :: term) :: handle_return
  @callback handle_message(client :: Client.t(), message :: binary) :: handle_return
  @callback handle_error(client :: Client.t(), error :: conn_error) :: handle_return
  @callback handle_halt_ok(client :: Client.t(), args :: term) :: handle_return
  @callback handle_halt_error(client :: Client.t(), error :: conn_error) :: handle_return

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
        {:ok, ranch_ip} = ip_address |> to_char_list() |> :inet.parse_ipv4_address()

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
      def handle_connection(client), do: {:ok, client}
      def handle_disconnection(client, _reason), do: {:ok, client}
      def handle_message(client, _message), do: {:ok, client}
      def handle_error(client, _reason), do: {:ok, client}
      def handle_halt_ok(client, _args), do: {:ok, client}
      def handle_halt_error(client, _reason), do: {:ok, client}

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
