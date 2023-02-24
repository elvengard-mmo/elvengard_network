defmodule EchoServer.Endpoint do
  @moduledoc """
  Documentation for EchoServer.Endpoint
  """

  use ElvenGard.Endpoint, otp_app: :echo_server

  require Logger

  ## Callbacks

  @impl true
  def init(type, config) do
    if type == :supervisor do
      host = get_in(config, [:transport_opts, :socket_opts, :ip])
      port = get_in(config, [:transport_opts, :socket_opts, :port])
      Logger.info("EchoServer started on #{:inet.ntoa(host)}:#{port}")
    end

    :ignore
  end
end
