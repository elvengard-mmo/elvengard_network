defmodule ElvenGard.Network.Endpoint.ThousandIsland do
  @moduledoc """
  Endpoint adapter using ThousandIsland
  """

  require Logger

  @behaviour ElvenGard.Network.Endpoint

  ## Endpoint behaviour

  @impl true
  def start_link(opts) do
    adapter_options = opts[:adapter_options] || []
    ip_address = Keyword.fetch!(opts, :ip_address)
    port = Keyword.fetch!(opts, :port)
    protocol = Keyword.fetch!(opts, :protocol)

    Logger.info(build_info(protocol, ip_address, port))

    thousand_island_opts =
      Keyword.merge(adapter_options,
        port: port,
        handler_module: __MODULE__.Handler,
        handler_options: [protocol: protocol]
      )

    ThousandIsland.start_link(thousand_island_opts)
  end

  ## Private functions

  defp build_info(protocol, ip_address, port) do
    server = "ThousandIsland #{Application.spec(:thousand_island)[:vsn]}"
    bind = "#{ip_address}:#{port}"
    "Running #{inspect(protocol)} with #{server} at #{bind}"
  end
end

defmodule ElvenGard.Network.Endpoint.ThousandIsland.Handler do
  @moduledoc false

  use ThousandIsland.Handler

  ## ThousandIsland.Handler behaviour

  @impl ThousandIsland.Handler
  def handle_connection(_socket, state) do
    IO.inspect(state, label: "state")
    {:continue, state}
  end

  @impl ThousandIsland.Handler
  def handle_data(data, socket, state) do
    IO.inspect(data, label: "data")
    ThousandIsland.Socket.send(socket, data)
    {:continue, state}
  end
end
