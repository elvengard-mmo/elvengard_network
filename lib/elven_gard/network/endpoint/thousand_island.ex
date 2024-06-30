defmodule ElvenGard.Network.Endpoint.ThousandIsland do
  @moduledoc """
  Endpoint adapter using ThousandIsland
  """

  require Logger

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
        handler_options: Keyword.fetch!(opts, :protocol),
        transport_module: transport_module!(opts),
        transport_options: transport_options!(opts)
      )
      |> IO.inspect()

    ThousandIsland.start_link(thousand_island_opts)
  end

  ## Private functions

  defp build_info(opts) do
    ip = Keyword.fetch!(opts, :ip)
    port = Keyword.fetch!(opts, :port)
    protocol = Keyword.fetch!(opts, :protocol)
    transport = Keyword.fetch!(opts, :transport)

    server = "ThousandIsland #{Application.spec(:thousand_island)[:vsn]}"
    bind = "#{ip}:#{port}"
    "Running #{inspect(protocol)} with #{server} at #{bind} (#{transport})"
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

  alias ElvenGard.Network.Socket

  @adapter ElvenGard.Network.Endpoint.ThousandIsland

  ## ThousandIsland.Handler behaviour

  @impl ThousandIsland.Handler
  def handle_connection(ti_socket, protocol) do
    socket = Socket.new(@adapter, ti_socket, protocol)

    init_error =
      "handle_connection/1 must return `{:ok, socket}`, `{:ok, socket, timeout}` " <>
        "or `{:stop, reason, new_socket}`"

    case protocol.handle_connection(socket) do
      {:ok, new_socket} -> {:continue, new_socket}
      {:ok, new_socket, timeout} -> {:continue, new_socket, timeout}
      {:stop, :normal, new_socket} -> {:close, new_socket}
      # TODO: Add :shutdown
      {:stop, reason, new_socket} -> {:error, reason, new_socket}
      _ -> raise init_error
    end
  end

  @impl ThousandIsland.Handler
  def handle_data(data, socket, state) do
    IO.inspect(data, label: "data")
    ThousandIsland.Socket.send(socket, data)
    {:continue, state}
  end
end