defmodule EchoServer.SocketHandler do
  @moduledoc """
  Documentation for EchoServer.SocketHandler
  """

  require Logger

  alias ElvenGard.Network.Socket

  @behaviour ElvenGard.Network.Endpoint.Protocol

  ## Callbacks

  @impl true
  def handle_connection(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    :ok = Socket.setopts(socket, packet: :line, reuseaddr: true)
    {:ok, socket}
  end

  @impl true
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message from #{socket.id} (len: #{byte_size(message)})")
    Socket.send(socket, "REPLY:" <> message)
    {:skip, socket}
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end
end
