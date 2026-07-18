defmodule LoginServer.Endpoint.Protocol do
  @moduledoc """
  Documentation for LoginServer.Endpoint.Protocol
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
    Logger.debug("New message from #{socket.id}: #{inspect(message)}")
    {:ok, socket}
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end
end
