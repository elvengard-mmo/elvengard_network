defmodule WorldServer.PacketHandler do
  @moduledoc """
  Received packet handler.
  """

  use ElvenGard.Helpers.Packet

  alias WorldServer.Actions.Auth

  #
  # Useless packets
  #

  @desc "Maybe a keep alive packet ?"
  useless_packet "0"

  useless_packet "c_close"
  useless_packet "f_stash_end"

  #
  # Usefull packets
  #

  @desc """
  First packet send by a client.
  Nedded for decrypt all following packets.

  /!\\ This packet doesn't have any packet header. Here, it't faked by Encoder
  """
  packet "session_id" do
    field :session_id, :integer
    resolve &Auth.process_session_id/2
  end

  @desc """
  Second packet send by a client.
  Contains only his username.

  /!\\ This packet doesn't have any packet header. Here, it't faked by Encoder
  """
  packet "username" do
    field :username, :string
    resolve &Auth.process_username/2
  end

  @desc """
  Third packet send by a client.
  Contains only his password in plain text.

  /!\\ This packet doesn't have any packet header. Here, it't faked by Encoder
  """
  packet "password" do
    field :password, :string
    resolve &Auth.process_password/2
  end
end
