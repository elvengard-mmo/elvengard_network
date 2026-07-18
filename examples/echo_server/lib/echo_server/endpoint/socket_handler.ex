defmodule EchoServer.Endpoint.SocketHandler do
  @moduledoc """
  Documentation for EchoServer.Endpoint.SocketHandler
  """

  use ElvenGard.Network.SocketHandler

  require Logger

  alias ElvenGard.Network.Socket

  ## Callbacks

  @impl true
  def handle_init(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    :ok = Socket.setopts(socket, packet: :line, reuseaddr: true)

    {:ok, socket}
  end

  @impl true
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message from #{socket.id} (len: #{byte_size(message)})")
    Socket.send(socket, "REPLY:" <> message)
    :ignore
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end
end
