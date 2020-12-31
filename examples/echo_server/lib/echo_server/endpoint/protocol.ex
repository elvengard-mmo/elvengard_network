defmodule EchoServer.Endpoint.Protocol do
  @moduledoc """
  Documentation for EchoServer.Endpoint.Protocol
  """

  use ElvenGard.Endpoint.Protocol

  require Logger

  ## Callbacks

  @impl true
  def handle_init(socket) do
    Logger.info("New connection: #{socket.id}")

    %{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :line, reuseaddr: true)

    {:ok, socket}
  end

  @impl true
  def handle_message(message, socket) do
    Logger.debug("New message from #{socket.id} (len: #{byte_size(message)})")

    %{transport: transport, transport_pid: transport_pid} = socket
    transport.send(transport_pid, message)

    :ignore
  end

  @impl true
  def handle_halt(reason, socket) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end
end
