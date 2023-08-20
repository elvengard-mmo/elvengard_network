defmodule MinecraftEx.ClientPackets do
  @moduledoc """
  TODO: MinecraftEx.ClientPackets
  """

  import ElvenGard.Network.PacketSerializer, only: [import_packets: 1]

  import_packets MinecraftEx.Client.HandshakePackets
  import_packets MinecraftEx.Client.LoginPackets
end
