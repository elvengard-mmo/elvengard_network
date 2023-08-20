defmodule LoginServer.Endpoint.PacketHandler do
  @moduledoc """
  LoginServer.Endpoint.PacketHandler
  """

  @behaviour ElvenGard.Network.PacketHandler

  alias ElvenGard.Network.Socket

  alias LoginServer.ClientPackets.{PingRequest, LoginRequest}
  alias LoginServer.PacketViews

  ## Handlers

  @impl true
  def handle_packet(%PingRequest{}, socket) do
    render = PacketViews.render(:pong_response, %{time: DateTime.utc_now()})
    :ok = Socket.send(socket, render)
    {:cont, socket}
  end

  def handle_packet(%LoginRequest{username: username, password: password}, socket) do
    render =
      if auth_using_db(username, password) do
        PacketViews.render(:login_succeed, %{world: get_worlds_from_manager()})
      else
        PacketViews.render(:login_failed, %{reason: "Bad credentials :/`"})
      end

    :ok = Socket.send(socket, render)
    {:halt, socket}
  end

  ## Fake functions

  defp auth_using_db(username, password) do
    case {username, password} do
      {"admin", "password"} -> true
      _ -> false
    end
  end

  defp get_worlds_from_manager() do
    %{host: "127.0.0.1", port: 5000}
  end
end
