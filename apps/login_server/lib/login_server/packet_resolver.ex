defmodule LoginServer.PacketResolver do
  @moduledoc """
  Parse a packet and call the right PacketHandler
  """

  use ElvenGard.Game.PacketResolver,
    packet_handler: LoginServer.PacketHandler

  require Logger
  alias LoginServer.Crypto

  @impl true
  def deserialize(data) do
    data
    |> Crypto.decrypt()
    |> String.replace("\n", "")
    |> String.split(:binary.compile_pattern([" ", "\v"]))
  end
end
