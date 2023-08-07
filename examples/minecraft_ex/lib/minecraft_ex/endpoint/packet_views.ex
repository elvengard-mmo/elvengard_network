defmodule MinecraftEx.Endpoint.PacketViews do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.PacketViews
  """

  use ElvenGard.Network.View

  alias MinecraftEx.Server.HandshakePackets.{PongResponse, StatusResponse}

  @impl true
  def render(:status_response, %{status: status}) do
    %StatusResponse{json: Poison.encode!(status)}
  end

  def render(:pong_response, %{payload: payload}) do
    %PongResponse{payload: payload}
  end
end
