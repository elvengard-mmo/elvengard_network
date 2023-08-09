defmodule MinecraftEx.Server.HandshakePackets do
  @moduledoc """
  Documentation for MinecraftEx.Server.HandshakePackets
  """

  use ElvenGard.Network.PacketSerializer

  alias MinecraftEx.Types.{Long, MCString}

  ## Handshake packets

  # 0x00 Handshake
  @serializable true
  defpacket 0x00, as: StatusResponse do
    field :json, MCString
  end

  # 0x00 Handshake
  @serializable true
  defpacket 0x01, as: PongResponse do
    field :payload, Long
  end
end
