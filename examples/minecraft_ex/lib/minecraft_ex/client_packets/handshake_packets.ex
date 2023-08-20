defmodule MinecraftEx.Client.HandshakePackets do
  @moduledoc """
  Documentation for MinecraftEx.Client.HandshakePackets
  """

  use ElvenGard.Network.PacketSerializer

  import MinecraftEx, only: [has_state: 2]

  alias MinecraftEx.Types.{
    Enum,
    Long,
    MCString,
    Short,
    VarInt
  }

  ## Handshake packets

  # 0x00 Handshake - state=init
  @deserializable true
  defpacket 0x00 when has_state(socket, :init), as: Handshake do
    field :protocol_version, VarInt
    field :server_address, MCString
    field :server_port, Short, sign: :unsigned
    field :next_state, Enum, from: VarInt, values: [status: 1, login: 2]
  end

  # 0x00 Status Request - state=status
  @deserializable true
  defpacket 0x00 when has_state(socket, :status), as: StatusRequest

  # 0x01 Ping Request - state=status
  @deserializable true
  defpacket 0x01 when has_state(socket, :status), as: PingRequest do
    field :payload, Long, sign: :signed
  end
end
