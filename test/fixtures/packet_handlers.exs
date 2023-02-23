Code.require_file("custom_types.exs", __DIR__)

defmodule MyApp.EmptyPacketHandler do
  use ElvenGard.PacketHandler
end

defmodule MyApp.WeirdPacketHandler do
  use ElvenGard.PacketHandler

  packet "WEIRD" do
    @desc "Weird desc"
  end

  packet "TEST", do: :ok
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

  packet "PING" do
    field :count, :integer
    resolve &ping_n_times/3
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
  def ping_n_times(_header, _args, _socket), do: :ping_n_times
end

defmodule MyApp.PingPacketHandlerExtension do
  use ElvenGard.PacketHandler

  packet "PING" do
    resolve &{:ping, &1, &2, &3}
  end
end

defmodule MyApp.LoginPacketHandlerExtension do
  use ElvenGard.PacketHandler

  ## Packet definitions

  ignore_packet "USELESS"

  packet "LOGIN" do
    field :username, :string, match: "admin"
    field :password, :string
    resolve &connect_admin/3
  end

  packet "LOGIN" do
    field :username, :string
    field :password, :string
    resolve &connect/3
  end

  ## Callbacks

  def connect(_header, _args, _socket), do: :connect
  def connect_admin(_header, _args, _socket), do: :connect_admin
end

defmodule MyApp.ExtendedPacketHandler do
  use ElvenGard.PacketHandler

  alias MyApp.{LoginPacketHandlerExtension, PingPacketHandlerExtension}

  defextension LoginPacketHandlerExtension, import: true
  defextension PingPacketHandlerExtension
end

defmodule MyApp.ExtendedDocPacketHandler do
  use ElvenGard.PacketHandler

  defextension MyApp.DocumentedPacketHandler
end
