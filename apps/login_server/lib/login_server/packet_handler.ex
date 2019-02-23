defmodule LoginServer.PacketHandler do
  @moduledoc """
  Received packet handler.
  """

  use ElvenGard.Helpers.Packet

  alias LoginServer.Actions.Auth

  @desc """
  The login packet

  TODO: Would be cool to have a custom field "password_field" to decryt pass
  """
  packet "NoS0575" do
    @desc "I thinks it's a session_id but is it really usefull ?"
    field :session, :integer

    field :username, :string

    @desc "Crypted for NostaleSE and in SHA512 for others"
    # field :password, :password_field
    field :password, :string

    @desc "A random string like `0039E3DC`. I don't known what it is"
    field :unknown, :string

    @desc "Something like `0.9.3.3071`"
    field :version, :string

    resolve &Auth.player_connect/2
  end
end
