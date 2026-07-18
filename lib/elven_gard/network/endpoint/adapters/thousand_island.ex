if Code.ensure_loaded?(ThousandIsland) do
  defmodule ElvenGard.Network.Endpoint.Adapters.ThousandIsland do
    @moduledoc """
    Thousand Island listener adapter for `ElvenGard.Network.Endpoint`.

    Projects using this adapter must add `:thousand_island` to their dependencies.
    """

    alias ElvenGard.Network.Endpoint
    alias ElvenGard.Network.Endpoint.IP
    alias ElvenGard.Network.Endpoint.Protocol
    alias ThousandIsland, as: ThousandIslandServer

    @behaviour Endpoint.Adapter

    ## Endpoint.Adapter callbacks

    @impl true
    @spec child_spec(module(), Endpoint.config(), Endpoint.runtime_options()) ::
            Supervisor.child_spec()
    def child_spec(endpoint, config, runtime_options) do
      config
      |> server_options(runtime_options)
      |> ThousandIslandServer.child_spec()
      |> Map.put(:id, {ThousandIslandServer, endpoint, listener_name(config)})
    end

    @impl true
    @spec get_addr(module(), Endpoint.config()) :: String.t()
    def get_addr(_endpoint, config) do
      {:ok, {address, _port}} =
        config
        |> listener_name()
        |> ThousandIslandServer.listener_info()

      address
      |> :inet.ntoa()
      |> List.to_string()
    end

    @impl true
    @spec get_port(module(), Endpoint.config()) :: :inet.port_number()
    def get_port(_endpoint, config) do
      {:ok, {_address, port}} =
        config
        |> listener_name()
        |> ThousandIslandServer.listener_info()

      port
    end

    ## Private function

    defp server_options(config, runtime_options) do
      config
      |> Keyword.fetch!(:adapter_options)
      |> Keyword.update(
        :supervisor_options,
        [name: listener_name(config)],
        &Keyword.put(&1, :name, listener_name(config))
      )
      |> Keyword.merge(
        handler_module: Protocol.ThousandIsland,
        handler_options: runtime_options,
        port: Keyword.fetch!(config, :port),
        transport_module: transport_module(config),
        transport_options: transport_options(config)
      )
    end

    defp listener_name(config) do
      Keyword.fetch!(config, :listener_name)
    end

    defp transport_module(config) do
      case Keyword.fetch!(config, :transport) do
        :tcp -> ThousandIsland.Transports.TCP
        :ssl -> ThousandIsland.Transports.SSL
      end
    end

    defp transport_options(config) do
      Keyword.merge(
        Keyword.fetch!(config, :transport_options),
        ip: config |> Keyword.fetch!(:ip) |> IP.normalize!()
      )
    end
  end
end
