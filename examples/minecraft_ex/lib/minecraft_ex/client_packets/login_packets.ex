defmodule MinecraftEx.Client.LoginPackets do
  @moduledoc """
  Documentation for MinecraftEx.Client.LoginPackets
  """

  use ElvenGard.Network.PacketSerializer

  import MinecraftEx, only: [has_state: 2]

  alias MinecraftEx.Types.{Boolean, MCString, UUID}

  ## Login packets

  # 0x00 Login Start - state=login
  @deserializable true
  defpacket 0x00 when has_state(socket, :login), as: LoginStart do
    field :name, MCString
    field :player_uuid?, Boolean
    field :player_uuid, UUID, if: packet.player_uuid?
  end
end
