defmodule EchoServer.Endpoint do
  @moduledoc """
  Documentation for EchoServer.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :echo_server

  require Logger

  ## Callbacks

  @impl ElvenGard.Network.Endpoint
  def handle_start(config) do
    host = Keyword.fetch!(config, :ip)
    port = Keyword.fetch!(config, :port)
    Logger.info("EchoServer started on #{host}:#{port}")
  end
end
