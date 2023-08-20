defmodule LoginServer.PacketViews do
  @moduledoc """
  Documentation for LoginServer.PacketViews
  """

  use ElvenGard.Network.View

  alias LoginServer.ServerPackets.{PongResponse, LoginFailed, LoginSucceed}
  alias LoginServer.SubPackets.WorldInfo

  @impl true
  def render(:pong_response, %{time: time}) do
    %PongResponse{time: time}
  end

  def render(:login_failed, %{reason: reason}) do
    %LoginFailed{reason: reason}
  end

  def render(:login_succeed, %{world: world}) do
    %LoginSucceed{world: %WorldInfo{host: world.host, port: world.port}}
  end
end
