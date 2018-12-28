defmodule HeavensStrike.Game.LoginServer do
  @moduledoc """
  Documentation for HeavensStrike.Game.LoginServer.
  """

  alias HeavensStrike.Game.Client

  @type conn_error :: atom | binary | bitstring

  @callback handle_init(args :: list) :: no_return
  @callback handle_connection(client :: Client.t()) :: no_return
  @callback handle_disconnection(client :: Client.t(), reason :: term) :: no_return
  @callback handle_message(client :: Client.t(), message :: binary) :: no_return
  @callback handle_client_accepted(client :: Client.t(), args :: term) :: no_return
  @callback handle_client_refused(client :: Client.t(), error :: conn_error) :: no_return

  @doc """
  Use HeavensStrike.Game.LoginServer behaviour.
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
      alias HeavensStrike.Game.Client

      @behaviour parent
      @behaviour :ranch_protocol

      @response_timeout 3000

      @doc false
      def child_spec(opts) do
        listener_name = __MODULE__
        num_acceptors = Application.get_env(:heavens_strike, :num_acceptors, 10)
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
          client = Client.new(socket, transport)
          handle_connection(client)
          recv_loop(client)
        end
      end

      #
      # Private function
      #

      defp recv_loop(%Client{} = client) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        tmp = transport.recv(socket, 0, @response_timeout)

        with {:ok, data} <- tmp do
          handle_message(client, data)

          case unquote(resolver).resolve(client, data) do
            {:cont} ->
              recv_loop(client)

            {:halt, {:ok, args}} ->
              accept_socket(client, args)

            {:halt, {:error, reason}} ->
              refuse_socket(client, reason)

            _ ->
              raise """
               	#{__MODULE__}.resolve/2 have to return `{:cont}`, \
                `{:halt, {:ok, :some_args}}`, or `{:halt, {:error, reason}}`.
              """
          end
        else
          {:error, reason} -> error_socket(client, reason)
        end
      end

      defp accept_socket(%Client{} = client, args) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        client |> handle_client_accepted(args)
        client |> handle_disconnection(:normal)
        socket |> transport.close()
      end

      defp refuse_socket(%Client{} = client, reason) do
        %Client{
          socket: socket,
          transport: transport
        } = client

        client |> handle_client_refused(reason)
        client |> handle_disconnection(:normal)
        socket |> transport.close()
      end

      defp error_socket(%Client{} = client, reason) do
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
      def handle_connection(_client), do: :unimplemented_function
      def handle_disconnection(_client, _reason), do: :unimplemented_function
      def handle_message(_client, _message), do: :unimplemented_function
      def handle_client_accepted(_client, _args), do: :unimplemented_function
      def handle_client_refused(_client, _reason), do: :unimplemented_function

      defoverridable handle_init: 1,
                     handle_connection: 1,
                     handle_disconnection: 2,
                     handle_message: 2,
                     handle_client_accepted: 2,
                     handle_client_refused: 2
    end
  end
end
