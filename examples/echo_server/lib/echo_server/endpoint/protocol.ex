defmodule EchoServer.Endpoint.Protocol do
  @moduledoc """
  Documentation for EchoServer.Endpoint.Protocol
  """

  use ElvenGard.Endpoint.Protocol

  require Logger

  alias ElvenGard.Socket

  ## Callbacks

  @impl true
  def handle_init(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")

    %Socket{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :line, reuseaddr: true)

    {:ok, socket}
  end

  @impl true
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message from #{socket.id} (len: #{byte_size(message)})")
    Socket.send(socket, message)
    :ignore
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end
end
