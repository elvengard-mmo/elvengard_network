defmodule MinecraftEx.Endpoint.PacketSchemas do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.PacketSchemas
  """

  use ElvenGard.Network.PacketSchema

  alias MinecraftEx.Types.{
    Boolean,
    Enum,
    Long,
    MCString,
    Short,
    UUID,
    VarInt
  }

  defguardp has_state(socket, state) when socket.assigns.state == state

  ## Handshake packets

  # 0x00 Handshake - state=init
  packet 0x00 when has_state(socket, :init), as: Handshake do
    field :protocol_version, VarInt
    field :server_address, MCString
    field :server_port, Short, sign: :unsigned
    field :next_state, Enum, from: VarInt, values: [status: 1, login: 2]
  end

  # 0x00 Status Request - state=status
  packet 0x00 when has_state(socket, :status), as: StatusRequest

  # 0x01 Ping Request - state=status
  packet 0x01 when has_state(socket, :status), as: PingRequest do
    field :payload, Long, sign: :signed
  end

  ## Login packets

  # 0x00 Login Start - state=login
  packet 0x00 when has_state(socket, :login), as: LoginStart do
    field :name, MCString
    field :player_uuid?, Boolean
    field :player_uuid, UUID, if: packet.player_uuid?
  end
end
