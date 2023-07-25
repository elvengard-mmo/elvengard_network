defmodule MinecraftEx.Endpoint.PacketViews do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.PacketViews
  """

  use ElvenGard.Network.View

  alias MinecraftEx.Types.{Long, MCString}

  @impl true
  def render(:status_response, %{json: json}) do
    packet(0x00, [MCString.encode(json)])
  end

  def render(:pong_response, %{payload: payload}) do
    packet(0x01, [Long.encode(payload)])
  end

  ## Helpers

  defp packet(id, params), do: [<<id::8>> | params]
end
