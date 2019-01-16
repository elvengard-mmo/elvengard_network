defmodule WorldServer.PacketResolver do
  @moduledoc """
  Parse a packet and call the right PacketHandler
  """

  use ElvenGard.Game.PacketResolver,
    packet_handler: WorldServer.PacketHandler

  require Logger
  alias WorldServer.Crypto

  def deserialize(data) do
    data
    # |> Crypto.decrypt()
    |> String.replace("\n", "")
    |> String.split(" ")
  end
end
