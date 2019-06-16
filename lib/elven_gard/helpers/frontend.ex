defmodule ElvenGard.Helpers.Frontend do
  @moduledoc """
  TODO: Documentation for ElvenGard.Helpers.Frontend
  """

  alias ElvenGard.Structures.Client

  @type conn_error :: atom | binary | bitstring
  @type handle_ok :: {:ok, Client.t()}
  @type handle_error :: {:error, term, Client.t()}
  @type handle_return :: handle_ok | handle_error
  @type state :: Client.t()

  @callback handle_init(args :: list) :: {:ok, term} | {:error, term}
  @callback handle_connection(socket :: identifier, transport :: atom) :: handle_return
  @callback handle_client_ready(client :: Client.t()) :: handle_return
  @callback handle_disconnection(client :: Client.t(), reason :: term) :: handle_return
  @callback handle_message(client :: Client.t(), message :: binary) :: handle_return
  @callback handle_error(client :: Client.t(), error :: conn_error) :: handle_return
  @callback handle_halt_ok(client :: Client.t(), args :: term) :: handle_return
  @callback handle_halt_error(client :: Client.t(), error :: conn_error) :: handle_return

  @doc """
  Use ElvenGard.Helpers.Frontend behaviour
  """
  defmacro __using__(opts) do
    parent = __MODULE__
    caller = __CALLER__.module
    port = get_in(opts, [:port]) || 3000
    protocol = get_in(opts, [:packet_protocol])
    handler = get_in(opts, [:packet_handler])
    use_opts = put_in(opts, [:port], port)

    # Check is there is any protocol
    unless protocol do
      raise "Please, specify a packet_protocol for #{caller}"
    end

    # Check is there is any handler
    unless handler do
      raise "Please, specify a packet_handler for #{caller}"
    end

    quote do
      alias ElvenGard.Structures.Client

      @behaviour unquote(parent)
      @behaviour :ranch_protocol

      # TODO: Use `Application.get_env` inside function for a live update
      @timeout Application.get_env(:elven_gard, :response_timeout, 2000)

      @doc false
      def child_spec(opts) do
        listener_name = __MODULE__
        num_acceptors = Application.get_env(:elven_gard, :num_acceptors, 10)
        transport = :ranch_tcp
        transport_opts = [port: unquote(port)]
        protocol = __MODULE__
        protocol_opts = []

        # TODO: Use args (pass them to ranch opts ?)
        {:ok, _args} =
          opts
          |> Enum.concat(unquote(use_opts))
          |> handle_init()

        :ranch.child_spec(
          listener_name,
          num_acceptors,
          transport,
          transport_opts,
          protocol,
          protocol_opts
        )
      end

      @doc false
      @impl true
      def start_link(ref, socket, transport, protocol_options) do
        opts = [
          ref,
          socket,
          transport,
          protocol_options
        ]

        pid = :proc_lib.spawn_link(__MODULE__, :init, opts)
        {:ok, pid}
      end

      @doc """
      Accept Ranch ack and handle packets
      """
      def init(ref, socket, transport, _) do
        with :ok <- :ranch.accept_ack(ref),
             :ok = transport.setopts(socket, [{:active, true}]),
             {:ok, client} <- handle_connection(socket, transport) do
          {:ok, final_client} =
            %Client{client | protocol: unquote(protocol)}
            |> handle_client_ready()

          :gen_server.enter_loop(__MODULE__, [], final_client, 10_000)
        end
      end

      #
      # All GenServer handles
      #

      def handle_info({:tcp, socket, data}, %Client{} = client) do
        # TODO: Manage errors on `handle_message`: don't execute the protocol
        {:ok, tmp_state} = handle_message(client, data)

        payload = unquote(protocol).complete_decode(data, tmp_state)

        case do_handle_packet(payload, tmp_state) do
          {:cont, final_client} ->
            {:noreply, final_client}

          {:halt, {:ok, args}, final_client} ->
            do_halt_ok(final_client, args)

          {:halt, {:error, reason}, final_client} ->
            do_halt_error(final_client, reason)

          x ->
            raise """
            #{unquote(handler)}.handle_packet/3 have to return `{:cont, client}`, \
            `{:halt, {:ok, :some_args}, client}`, or `{:halt, {:error, reason}, client} `. \
            Returned: #{inspect(x)}
            """
        end
      end

      def handle_info({:tcp_closed, _socket}, %Client{} = client) do
        {:ok, new_state} = handle_disconnection(client, :normal)
        {:stop, :normal, new_state}
      end

      def handle_info({:tcp_error, _socket, reason}, %Client{} = client) do
        {:ok, new_state} = handle_error(client, reason)
        {:stop, reason, new_state}
      end

      def handle_info(:timeout, %Client{} = client) do
        {:ok, new_state} = handle_error(client, :timeout)
        {:stop, :normal, new_state}
      end

      #
      # Private function
      #

      @spec do_handle_packet({term, map} | list(tuple), Client.t()) ::
              {:cont, unquote(parent).state}
              | {:halt, {:ok, term}, unquote(parent).state}
              | {:halt, {:error, unquote(parent).conn_error()}, unquote(parent).state}
      defp do_handle_packet({header, params}, client) do
        unquote(handler).handle_packet(header, params, client)
      end

      defp do_handle_packet([{_header, _params} | _t] = packet_list, client) do
        Enum.reduce_while(packet_list, {:cont, client}, fn packet, {_, client} ->
          res = do_handle_packet(packet, client)
          {elem(res, 0), res}
        end)
      end

      defp do_handle_packet(x, _client) do
        raise """
        Unable to handle packet #{inspect(x)}.
        Please check that your protocol returns a tuple in the form of {header, \
        %{param1: :val1, param2: :val2, ...} or a list of tuples
        """
      end

      @spec do_halt_ok(Client.t(), term) :: {:stop, :normal, Client.t()}
      defp do_halt_ok(%Client{} = client, args) do
        final_client =
          client
          |> handle_halt_ok(args)
          |> close_socket(:normal)

        {:stop, :normal, final_client}
      end

      @spec do_halt_error(Client.t(), term) :: {:stop, :normal, Client.t()}
      defp do_halt_error(%Client{} = client, reason) do
        final_client =
          client
          |> handle_halt_error(reason)
          |> close_socket(reason)

        {:stop, :normal, final_client}
      end

      @spec close_socket(unquote(parent).handle_return(), term) :: Client.t()
      defp close_socket({:ok, %Client{} = client}, reason), do: do_close_socket(client, reason)

      defp close_socket({:error, _, %Client{} = client}, reason),
        do: do_close_socket(client, reason)

      @spec do_close_socket(Client.t(), term) :: Client.t()
      defp do_close_socket(%Client{} = client, reason) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        {:ok, final_client} = handle_disconnection(client, reason)
        transport.close(socket)
        final_client
      end

      #
      # Default implementations
      #

      def handle_init(_args), do: {:ok, nil}
      def handle_connection(socket, transport), do: Client.new(socket, transport)
      def handle_client_ready(client), do: {:ok, client}
      def handle_disconnection(client, _reason), do: {:ok, client}
      def handle_message(client, _message), do: {:ok, client}
      def handle_error(client, _reason), do: {:ok, client}
      def handle_halt_ok(client, _args), do: {:ok, client}
      def handle_halt_error(client, _reason), do: {:ok, client}

      defoverridable handle_init: 1,
                     handle_connection: 2,
                     handle_client_ready: 1,
                     handle_disconnection: 2,
                     handle_message: 2,
                     handle_error: 2,
                     handle_halt_ok: 2,
                     handle_halt_error: 2
    end
  end
end
