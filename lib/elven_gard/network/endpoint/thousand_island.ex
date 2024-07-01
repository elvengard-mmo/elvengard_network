if Code.ensure_loaded?(ThousandIsland) do
  defmodule ElvenGard.Network.Endpoint.ThousandIsland do
    @moduledoc """
    Endpoint adapter using ThousandIsland
    """

    require Logger

    alias ElvenGard.Network.Endpoint

    @behaviour ElvenGard.Network.Endpoint

    ## Endpoint behaviour

    @impl true
    def start_link(opts) do
      Logger.info(build_info(opts))

      thousand_island_opts =
        opts
        |> Keyword.get(:adapter_options, [])
        |> Keyword.merge(
          port: Keyword.fetch!(opts, :port),
          handler_module: __MODULE__.Handler,
          handler_options: Keyword.fetch!(opts, :socket_handler),
          transport_module: transport_module!(opts),
          transport_options: transport_options!(opts)
        )

      ThousandIsland.start_link(thousand_island_opts)
    end

    @impl true
    def setopts(ti_socket, opts) do
      ThousandIsland.Socket.setopts(ti_socket, opts)
    end

    ## Private functions

    defp build_info(opts) do
      server = "ThousandIsland #{Application.spec(:thousand_island)[:vsn]}"
      Endpoint.build_info(server, opts)
    end

    defp transport_module!(opts) do
      case Keyword.get(opts, :transport) do
        :tcp -> ThousandIsland.Transports.TCP
        :ssl -> ThousandIsland.Transports.SSL
        transport -> raise "transport #{inspect(transport)} not supported by ThousandIsland"
      end
    end

    defp transport_options!(opts) do
      ip =
        opts
        |> Keyword.fetch!(:ip)
        |> String.to_charlist()
        |> :inet.parse_address()
        |> then(fn {:ok, tuple} -> tuple end)

      opts
      |> Keyword.get(:transport_options, [])
      |> Keyword.put(:ip, ip)
    end
  end

  defmodule ElvenGard.Network.Endpoint.ThousandIsland.Handler do
    @moduledoc false

    use ThousandIsland.Handler

    alias ElvenGard.Network.{Endpoint, Socket}

    @adapter ElvenGard.Network.Endpoint.ThousandIsland

    ## ThousandIsland.Handler behaviour

    @impl ThousandIsland.Handler
    def handle_connection(ti_socket, socket_handler) do
      socket = Socket.new(@adapter, ti_socket, socket_handler)

      case socket_handler.handle_connection(socket) do
        {:ok, new_socket} -> {:continue, new_socket}
        {:ok, new_socket, timeout} -> {:continue, new_socket, timeout}
        {:stop, :normal, new_socket} -> {:close, new_socket}
        {:stop, reason, new_socket} -> {:error, {:stop, reason}, new_socket}
      end
    end

    @impl ThousandIsland.Handler
    def handle_data(data, _ti_socket, socket) do
      %Socket{remaining: remaining, handler: socket_handler} = socket
      full_data = <<remaining::bitstring, data::bitstring>>

      case socket_handler.handle_data(full_data, socket) do
        {:ok, new_socket} -> packet_loop(full_data, new_socket)
        {:skip, new_socket} -> {:continue, new_socket}
        {:stop, :normal, new_socket} -> {:close, new_socket}
        {:stop, reason, new_socket} -> {:error, {:stop, reason}, new_socket}
        term -> raise "invalid return value for handle_data/2 (got: #{inspect(term)})"
      end
    end

    @impl ThousandIsland.Handler
    def handle_close(_ti_socket, socket) do
      %Socket{handler: socket_handler} = socket
      socket_handler.handle_close(socket)
    end

    @impl ThousandIsland.Handler
    def handle_error(reason, _ti_socket, socket) do
      %Socket{handler: socket_handler} = socket
      socket_handler.handle_error(reason, socket)
    end

    @impl ThousandIsland.Handler
    def handle_shutdown(_ti_socket, socket) do
      %Socket{handler: socket_handler} = socket
      socket_handler.handle_shutdown(socket)
    end

    @impl ThousandIsland.Handler
    def handle_timeout(_ti_socket, socket) do
      %Socket{handler: socket_handler} = socket
      socket_handler.handle_timeout(socket)
    end

    ## Private functions

    defp packet_loop(data, socket) do
      case Endpoint.packet_loop(data, socket) do
        {:ok, socket} -> {:continue, socket}
        {:stop, :normal, socket} -> {:close, socket}
        {:stop, reason, socket} -> {:error, {:stop, reason}, socket}
      end
    end
  end
end
