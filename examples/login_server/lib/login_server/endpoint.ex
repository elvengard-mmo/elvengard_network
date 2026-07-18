defmodule LoginServer.Endpoint do
  @moduledoc """
  Documentation for LoginServer.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :login_server

  require Logger

  ## Callbacks

  @impl true
  def handle_start(config) do
    host = Keyword.fetch!(config, :ip)
    port = Keyword.fetch!(config, :port)
    Logger.info("LoginServer started on #{host}:#{port}")
  end
end
