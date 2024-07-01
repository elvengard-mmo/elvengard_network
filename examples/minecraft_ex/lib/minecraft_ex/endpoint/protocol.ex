defmodule MinecraftEx.Endpoint.Protocol do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.Protocol
  """

  use ElvenGard.Network.Endpoint.Protocol

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

  alias ElvenGard.Network.Socket

  ## Callbacks

  @impl true
  def handle_connection(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    Logger.metadata(socket_id: socket.id)

    %Socket{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :raw, reuseaddr: true)

    {:ok, assign(socket, :state, :init)}
  end

  @impl true
  def handle_data(message, %Socket{} = socket) do
    Logger.debug("New message (len: #{byte_size(message)})")
    {:ok, socket}
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
