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
