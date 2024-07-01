defmodule EchoServer.SocketHandler do
  @moduledoc """
  Documentation for EchoServer.SocketHandler
  """

  require Logger

  use ElvenGard.Network.SocketHandler

  alias ElvenGard.Network.Socket


  ## Callbacks

  @impl true
  def handle_connection(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    :ok = Socket.setopts(socket, packet: :line, reuseaddr: true)
    {:ok, socket}
  end

  @impl true
  def handle_data(message, %Socket{} = socket) do
    Logger.debug("New message from #{socket.id} (len: #{byte_size(message)})")
    Socket.send(socket, "REPLY:" <> message)
    {:skip, socket}
  end

  @impl true
  def handle_close(%Socket{} = socket) do
    info_disconnected(socket, :closed)
  end

  @impl true
  def handle_error(reason, %Socket{} = socket) do
    info_disconnected(socket, reason)
  end

  @impl true
  def handle_shutdown(%Socket{} = socket) do
    info_disconnected(socket, :shutdown)
  end

  @impl true
  def handle_timeout(%Socket{} = socket) do
    info_disconnected(socket, :timeout)
  end

  ## Private functions

  defp info_disconnected(socket, reason) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
  end
end
