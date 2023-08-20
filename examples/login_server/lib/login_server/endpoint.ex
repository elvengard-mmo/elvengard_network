defmodule LoginServer.Endpoint do
  @moduledoc """
  Documentation for LoginServer.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :login_server

  require Logger

  ## Callbacks

  @impl true
  def handle_start(config) do
    host = get_in(config, [:transport_opts, :socket_opts, :ip])
    port = get_in(config, [:transport_opts, :socket_opts, :port])
    Logger.info("LoginServer started on #{:inet.ntoa(host)}:#{port}")
  end
end
