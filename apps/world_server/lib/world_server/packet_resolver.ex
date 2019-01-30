defmodule WorldServer.PacketResolver do
  @moduledoc """
  Parse a packet and call the right PacketHandler
  """

  use ElvenGard.Helpers.PacketResolver,
    packet_handler: WorldServer.PacketHandler

  require Logger
  alias WorldServer.Crypto
  alias ElvenGard.Structures.Client

  @impl true
  def resolve(%Client{} = client, data) do
    auth_step = Client.get_metadata(client, :auth_step)
    session_id = Client.get_metadata(client, :session_id)

    {auth_step, data, session_id}
    |> deserialize()
    |> handle_packet(client)
  end

  @impl true
  def deserialize({:default, data, session_id}) do
    data
    |> Crypto.decrypt(session_id)
    |> Stream.map(& String.replace(&1, "\n", ""))
    |> Enum.map(& String.split(&1, " "))
  end

  @impl true
  def deserialize({:waiting_session, data, nil}) do
    [Crypto.decrypt_session(data)]
  end
end
