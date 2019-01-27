defmodule ElvenGard.Game.Frontend do
  @moduledoc """
  Documentation for ElvenGard.Game.Frontend.
  """

  alias ElvenGard.Game.Client

  @type conn_error :: atom | binary | bitstring

  @callback handle_init(args :: list) :: no_return
  @callback handle_connection(socket :: identifier, transport :: atom) :: Client.t()
  @callback handle_disconnection(client :: Client.t(), reason :: term) :: no_return
  @callback handle_message(client :: Client.t(), message :: binary) :: no_return
  @callback handle_halt_ok(client :: Client.t(), args :: term) :: no_return
  @callback handle_halt_error(client :: Client.t(), error :: conn_error) :: no_return

  @doc """
  Use ElvenGard.Game.Frontend behaviour.
  """
  defmacro __using__(opts) do
    parent = __MODULE__
    caller = __CALLER__.module
    port = get_in(opts, [:port]) || 3000
    resolver = get_in(opts, [:packet_resolver])
    use_opts = put_in(opts, [:port], port)

    # Check is there is any resolver
    case resolver do
      nil -> raise "Please, specify a packet_resolver for #{caller}"
      _ -> :ok
    end

    quote bind_quoted: [
            use_opts: use_opts,
            port: port,
            parent: parent,
            resolver: resolver
          ] do
      alias ElvenGard.Game.Client

      @behaviour parent
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
        with :ok <- :ranch.accept_ack(ref) do
          client = handle_connection(socket, transport)
          recv_loop(client)
        end
      end

      #
      # Private function
      #

      @spec recv_loop(Client.t()) :: no_return
      defp recv_loop(%Client{} = client) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        tmp = transport.recv(socket, 0, @timeout)

        with {:ok, data} <- tmp do
          handle_message(client, data)

          case unquote(resolver).resolve(client, data) do
            {:cont} ->
              recv_loop(client)

            {:halt, {:ok, args}} ->
              do_halt_ok(client, args)

            {:halt, {:error, reason}} ->
              do_halt_error(client, reason)

            _ ->
              raise """
               	#{__MODULE__}.resolve/2 have to return `{:cont}`, \
                `{:halt, {:ok, :some_args}}`, or `{:halt, {:error, reason}}`.
              """
          end
        else
          {:error, reason} -> close_socket(client, reason)
        end
      end

      @spec do_halt_ok(Client.t(), term) :: no_return
      defp do_halt_ok(%Client{} = client, args) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        client |> handle_halt_ok(args)
        client |> close_socket(:normal)
      end

      @spec do_halt_error(Client.t(), term) :: no_return
      defp do_halt_error(%Client{} = client, reason) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        client |> handle_halt_error(reason)
        client |> close_socket(reason)
      end

      @spec close_socket(Client.t(), term) :: no_return
      defp close_socket(%Client{} = client, reason) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        client |> handle_disconnection(reason)
        socket |> transport.close()
      end

      #
      # Default implementations
      #

      def handle_init(_args), do: :unimplemented_function
      def handle_connection(socket, transport), do: Client.new(socket, transport)
      def handle_disconnection(_client, _reason), do: :unimplemented_function
      def handle_message(_client, _message), do: :unimplemented_function
      def handle_halt_ok(_client, _args), do: :unimplemented_function
      def handle_halt_error(_client, _reason), do: :unimplemented_function

      defoverridable handle_init: 1,
                     handle_connection: 2,
                     handle_disconnection: 2,
                     handle_message: 2,
                     handle_halt_ok: 2,
                     handle_halt_error: 2
    end
  end
end
