defmodule LoginServer.ClientPackets do
  @moduledoc """
  Documentation for LoginServer.ClientPackets
  """

  use ElvenGard.Network.PacketSerializer

  alias LoginServer.Types.StringType

  ## Ping packet

  @deserializable true
  defpacket "PING", as: PingRequest

  ## Login packet

  @deserializable true
  defpacket "LOGIN", as: LoginRequest do
    field :username, StringType
    field :password, StringType
  end
end
