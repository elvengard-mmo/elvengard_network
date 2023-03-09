defmodule EchoServer.Endpoint do
  @moduledoc """
  Documentation for EchoServer.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :echo_server

  require Logger

  ## Callbacks

  @impl ElvenGard.Network.Endpoint
  def handle_start(config) do
    host = get_in(config, [:transport_opts, :socket_opts, :ip])
    port = get_in(config, [:transport_opts, :socket_opts, :port])
    Logger.info("EchoServer started on #{:inet.ntoa(host)}:#{port}")
  end
end
