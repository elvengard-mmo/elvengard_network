defmodule LoginServer.PacketHandler do
  @moduledoc """
  Received packet handler.
  """

  use ElvenGard.Helpers.Packet

  alias LoginServer.Actions.Auth

  packet "NoS0575" do
    field :session, :integer
    field :username, :string
    field :password, :string
    field :unknown, :string
    field :version, :string
    resolve &Auth.player_connect/2
  end
end
