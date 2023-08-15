defmodule LoginServer.ServerPackets do
  @moduledoc """
  Documentation for LoginServer.ServerPackets
  """

  use ElvenGard.Network.PacketSerializer

  alias LoginServer.Types.{DateTimeType, StringType}
  alias LoginServer.SubPackets.WorldInfo

  ## Ping packet

  @serializable true
  defpacket "PONG", as: PongResponse do
    field :time, DateTimeType
  end

  ## Login packets

  @serializable true
  defpacket "FAIL", as: LoginFailed do
    field :reason, StringType
  end

  @serializable true
  defpacket "SUCCESS", as: LoginSucceed do
    field :world, WorldInfo, sep: ":"
  end
end
