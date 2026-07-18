defmodule ElvenGard.Network.Endpoint.Config do
  @moduledoc false

  alias ElvenGard.Network.Endpoint

  ## Public API

  @doc """
  Returns the endpoint configuration used at compile time.
  """
  @spec config(atom(), module(), Endpoint.options()) :: Endpoint.config()
  def config(otp_app, endpoint, configured_options) do
    config = Keyword.merge(defaults(otp_app, endpoint), configured_options)
    _adapter = Keyword.fetch!(config, :adapter)
    _socket_handler = Keyword.fetch!(config, :socket_handler)
    config
  end

  @doc """
  Resolves the modules used by every connection started by an endpoint.
  """
  @spec runtime_options(Endpoint.config()) :: Endpoint.runtime_options()
  def runtime_options(config) do
    otp_app = Keyword.fetch!(config, :otp_app)
    socket_handler = Keyword.fetch!(config, :socket_handler)
    handler_config = Application.fetch_env!(otp_app, socket_handler)

    [
      socket_handler: socket_handler,
      network_codec: Keyword.fetch!(handler_config, :network_codec),
      packet_handler: Keyword.fetch!(handler_config, :packet_handler)
    ]
  end

  ## Private function

  defp defaults(otp_app, endpoint) do
    [
      otp_app: otp_app,
      adapter_options: [],
      listener_name: endpoint,
      ip: {127, 0, 0, 1},
      port: 3000,
      transport: :tcp,
      transport_options: []
    ]
  end
end
