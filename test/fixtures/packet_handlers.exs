Code.require_file("custom_types.exs", __DIR__)

defmodule MyApp.EmptyPacketHandler do
  use ElvenGard.PacketHandler
end

defmodule MyApp.DefaultIgnorePacketHandler do
  use ElvenGard.PacketHandler

  ignore_packet "USELESS"
end

defmodule MyApp.CustomTypePacketHandler do
  use ElvenGard.PacketHandler

  alias MyApp.BasicType

  packet "TEST" do
    field :field, BasicType
  end
end

defmodule MyApp.DocumentedPacketHandler do
  use ElvenGard.PacketHandler

  @desc """
  Packet ignored
  """
  ignore_packet "USELESS"

  @desc "Simple ping"
  packet "PING" do
    field :target, :string
    @desc "Counter"
    field :count, :integer, optional: true, some_tag: 1
  end
end

defmodule MyApp.SimplePacketHandler do
  use ElvenGard.PacketHandler

  ## Packet definitions

  ignore_packet "USELESS"

  packet "PING" do
    resolve &ping/3
  end

  packet "LOGIN" do
    field :username, :string, match: "admin"
    field :password, :string, optional: true
    resolve &connect_admin/3
  end

  packet "LOGIN" do
    field :username, :string
    field :password, :string
    resolve &connect/3
  end

  packet "LOGIN" do
    field :username, :string
    field :token_2fa, :string
    resolve &connect_2fa/3
  end

  packet "LOGIN" do
    field :username, :string
    resolve &connect_user/3
  end

  default_packet_callback do
    {:cont, {header, args, socket}}
  end

  ignore_packet_callback do
    {:halt, {header, args, socket}}
  end

  ## Callbacks

  def ping(header, args, socket), do: {:ping, header, args, socket}
  def connect(_header, _args, _socket), do: :connect
  def connect_2fa(_header, _args, _socket), do: :connect_2fa
  def connect_admin(_header, _args, _socket), do: :connect_admin
  def connect_user(_header, _args, _socket), do: :connect_user
end
