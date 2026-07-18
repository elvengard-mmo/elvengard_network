defmodule MinecraftEx.Endpoint do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :minecraft_ex

  require Logger

  ## Callbacks

  @impl true
  def handle_start(config) do
    host = Keyword.fetch!(config, :ip)
    port = Keyword.fetch!(config, :port)
    Logger.info("MinecraftEx started on #{host}:#{port}")
  end
end
