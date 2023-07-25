defmodule MinecraftEx.Endpoint do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :minecraft_ex

  require Logger

  ## Callbacks

  @impl true
  def handle_start(config) do
    host = get_in(config, [:transport_opts, :socket_opts, :ip])
    port = get_in(config, [:transport_opts, :socket_opts, :port])
    Logger.info("MinecraftEx started on #{:inet.ntoa(host)}:#{port}")
  end
end
