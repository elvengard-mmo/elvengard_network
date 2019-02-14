defmodule WorldServer.PacketResolver do
  @moduledoc """
  Parse a packet and call the right PacketHandler
  """

  use ElvenGard.Helpers.PacketResolver,
    packet_handler: WorldServer.PacketHandler

  require Logger

  alias ElvenGard.Structures.Client
  alias WorldServer.Crypto

  @impl true
  def resolve(%Client{} = client, data) do
    auth_step = Client.get_metadata(client, :auth_step)
    session_id = Client.get_metadata(client, :session_id)

    # Here, the "PacketResolver" helper provide the "handle_packet" function
    {auth_step, data, session_id}
    |> deserialize()
    |> handle_packet(client)
  end

  @impl true
  def deserialize({:done, data, session_id}) do
    data
    |> Crypto.decrypt(session_id)
    |> Stream.map(& String.replace(&1, "\n", ""))
    |> Enum.map(& String.split(&1, " "))
  end

  @impl true
  def deserialize({:waiting_session, data, nil}) do
    # Place fake packet header
    ["session_id", Crypto.decrypt_session(data)]
  end

  @impl true
  def deserialize({:waiting_username, data, session_id}) do
    # Place fake packet header
    ["username", Crypto.decrypt(data, session_id)]
  end

  @impl true
  def deserialize({:waiting_password, data, session_id}) do
    # Place fake packet header
    ["password", Crypto.decrypt(data, session_id)]
  end
end
