defmodule LoginServer.Frontend do
  @moduledoc """
  Documentation for LoginServer.Frontend.
  """

  use ElvenGard.Helpers.Frontend,
    packet_encoder: LoginServer.PacketEncoder,
    packet_handler: LoginServer.PacketHandler,
    port: 4002

  require Logger
  alias ElvenGard.Structures.Client

  @impl true
  def handle_init(args) do
    port = get_in(args, [:port])
    Logger.info("Login server started on port #{port}")
    {:ok, nil}
  end

  @impl true
  def handle_connection(socket, transport) do
    client = Client.new(socket, transport)
    Logger.info("New connection: #{client.id}")
    {:ok, client}
  end

  @impl true
  def handle_disconnection(%Client{id: id} = client, reason) do
    Logger.info("#{id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, client}
  end

  @impl true
  def handle_message(%Client{id: id} = client, message) do
    Logger.info("New message from #{id} (len: #{byte_size(message)})")
    {:ok, client}
  end

  @impl true
  def handle_error(%Client{id: id} = client, reason) do
    Logger.error("An error occured with client #{id}: #{inspect(reason)}")
    {:ok, client}
  end

  @impl true
  def handle_halt_ok(%Client{id: id} = client, args) do
    Logger.info("Client accepted: #{id}")
    Client.send(client, "#{inspect(args)}")
    {:ok, client}
  end

  @impl true
  def handle_halt_error(%Client{id: id} = client, reason) do
    Logger.info("Client refused: #{id} - #{inspect(reason)}")
    Client.send(client, "fail #{inspect(reason)}")
    {:ok, client}
  end
end
