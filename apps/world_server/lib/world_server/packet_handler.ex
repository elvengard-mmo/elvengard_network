defmodule WorldServer.PacketHandler do
  @moduledoc """
  Received packet handler.
  """

  use ElvenGard.Helpers.Packet

  alias WorldServer.Actions.Auth

  # Useless packets
	useless_packet "0"
	useless_packet "c_close"
  useless_packet "f_stash_end"

  # Usefull packets
  packet "session_id" do
    field :session_id, :integer
    resolve &Auth.process_session_id/2
  end

  packet "username" do
    field :username, :string
    resolve &Auth.process_username/2
  end

  # TODO: Would be cool to have a custom field "password_field" to decryt pass
  packet "password" do
    # field :password, :password_field
    field :password, :string
    resolve &Auth.process_password/2
  end
end
