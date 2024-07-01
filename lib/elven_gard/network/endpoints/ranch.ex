if Code.ensure_loaded?(:ranch) do
  defmodule ElvenGard.Network.Endpoints.Ranch do
    @moduledoc """
    Endpoint adapter using Ranch
    """

    require Logger

    alias ElvenGard.Network.Endpoint

    @behaviour ElvenGard.Network.Endpoint

    ## Endpoint behaviour

    @impl true
    def start_link(opts) do
      Logger.info(build_info(opts))
      Supervisor.start_link([{__MODULE__.Listener, opts}], strategy: :one_for_one)
    end

    @impl true
    def setopts({transport, conn}, opts) do
      transport.setopts(conn, opts)
    end

    ## Private functions

    defp build_info(opts) do
      server = "Ranch #{Application.spec(:ranch)[:vsn]}"
      Endpoint.build_info(server, opts)
    end
  end

  defmodule ElvenGard.Network.Endpoints.Ranch.Listener do
    @moduledoc false

    alias ElvenGard.Network.Endpoint

    @adapter ElvenGard.Network.Endpoints.Ranch

    ## Public API

    @spec child_spec(Endpoint.options()) :: :supervisor.child_spec()
    def child_spec(opts) do
      socket_handler = Keyword.fetch!(opts, :socket_handler)

      :ranch.child_spec(
        {__MODULE__, socket_handler},
        transport_module!(opts),
        transport_options!(opts),
        @adapter.Protocol,
        socket_handler
      )
    end

    ## Private API

    defp transport_module!(opts) do
      case Keyword.get(opts, :transport) do
        :tcp -> :ranch_tcp
        :ssl -> :ranch_ssl
        transport -> raise "transport #{inspect(transport)} not supported by Ranch"
      end
    end

    defp transport_options!(opts) do
      # FIXME: use same default as ThousandIsland
      port = Keyword.fetch!(opts, :port)

      ip =
        opts
        |> Keyword.fetch!(:ip)
        |> String.to_charlist()
        |> :inet.parse_address()
        |> then(fn {:ok, tuple} -> tuple end)

      socket_opts_access = Access.key(:socket_opts, [])

      opts
      |> Keyword.get(:transport_options, [])
      |> Map.new()
      |> put_in([socket_opts_access, :ip], ip)
      |> put_in([socket_opts_access, :port], port)
    end
  end

  defmodule ElvenGard.Network.Endpoints.Ranch.Protocol do
    @moduledoc false

    use GenServer

    alias ElvenGard.Network.Socket

    @behaviour :ranch_protocol

    @adapter ElvenGard.Network.Endpoints.Ranch

    ## ranch_protocol behaviour

    @impl :ranch_protocol
    def start_link(ref, transport, socket_handler) do
      {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, socket_handler}])}
    end

    ## GenServer behaviour

    @impl GenServer
    def init({ref, transport, socket_handler}) do
      {:ok, conn} = :ranch.handshake(ref)

      state = {transport, conn}
      socket = Socket.new(@adapter, state)

      init_error =
        "handle_init/1 must return `{:ok, socket}`, `{:ok, socket, timeout}` " <>
          "or `{:stop, reason, new_socket}`"

      case socket_handler.handle_connection(socket) do
        {:ok, new_socket} -> do_enter_loop(new_socket)
        {:ok, new_socket, timeout} -> do_enter_loop(new_socket, timeout)
        # FIXME: call handle_error, handle_halt callbacks
        {:stop, :normal, _new_socket} -> {:stop, :normal}
        {:stop, reason, _new_socket} -> {:stop, reason}
        _ -> raise init_error
      end
    end

    ## Helpers

    defp do_enter_loop(socket, timeout \\ :infinity) do
      %Socket{adapter_state: {transport, conn}} = socket
      :ok = transport.setopts(conn, active: :once)
      :gen_server.enter_loop(__MODULE__, [], socket, timeout)
    end
  end
end
