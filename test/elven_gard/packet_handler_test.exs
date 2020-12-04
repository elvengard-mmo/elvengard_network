Code.require_file("../fixtures/packet_handlers.exs", __DIR__)

defmodule ElvenGard.PacketHandlerTest do
  use ExUnit.Case, async: true

  alias ElvenGard.PacketHandler.{FieldDefinition, PacketDefinition}

  ## General behaviour

  describe "__using__/1" do
    test "generate a default packet callback" do
      assert MyApp.EmptyPacketHandler.handle_packet("UNKNOWN", %{}, 123) ==
               {:halt, {:invalid, "UNKNOWN"}, 123}
    end

    test "generate a default ignore callback" do
      assert MyApp.EmptyPacketHandler.handle_ignore("USELESS", %{}, 123) == {:cont, 123}
    end
  end

  ## Ignore callbacks

  describe "ignore_packet/1" do
    test "delegate to `c:handle_ignore/3` callback" do
      assert MyApp.DefaultIgnorePacketHandler.handle_packet("USELESS", %{}, 123) == {:cont, 123}
    end
  end

  describe "ignore_packet_callback/1" do
    test "can override the default ignore callback" do
      assert simple_handle_packet("USELESS", %{}, 123) == {:halt, {"USELESS", %{}, 123}}
    end
  end

  ## Packet callbacks

  describe "default_packet_callback/1" do
    test "can override the default packet handler" do
      assert simple_handle_packet("UNKNOWN", %{}, 123) == {:cont, {"UNKNOWN", %{}, 123}}
    end
  end

  describe "resolve/1" do
    test "create a handle_packet/3 callback" do
      assert simple_handle_packet("PING", %{}, nil) == {:ping, "PING", %{}, nil}
    end
  end

  describe "field/3" do
    test "pattern match with args names" do
      args = %{username: "username", password: "password"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect
    end

    test "pattern match same header but differents args names" do
      args = %{username: "username", token_2fa: "token_2fa"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_2fa
    end

    test "pattern match same header but less args" do
      args = %{username: "username"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_user
    end

    test "pattern match same header but with matching arg" do
      args = %{username: "admin", password: "password"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_admin
    end

    test "pattern match same header but with optional arg" do
      args = %{username: "admin"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_admin
    end
  end

  ## Introspection

  test "context is cleared for each new packets" do
    expected = [
      %PacketDefinition{header: "WEIRD", description: nil, tags: [], fields: []},
      %PacketDefinition{header: "TEST", description: nil, tags: [], fields: []}
    ]

    assert MyApp.WeirdPacketHandler.__defs__() == expected
  end

  describe "__defs__/0" do
    test "returns an empty array if no packet handler" do
      assert MyApp.EmptyPacketHandler.__defs__() == []
    end

    test "support custom types" do
      expected = [
        %PacketDefinition{
          header: "TEST",
          description: nil,
          tags: [],
          fields: [
            %FieldDefinition{name: :field, type: MyApp.BasicType, description: nil, opts: []}
          ]
        }
      ]

      assert MyApp.CustomTypePacketHandler.__defs__() == expected
    end

    test "returns a correct structure for multiple definitions" do
      expected = [
        %PacketDefinition{
          header: "USELESS",
          description: "Packet ignored",
          tags: [:ignored],
          fields: []
        },
        %PacketDefinition{
          header: "PING",
          description: "Simple ping",
          tags: [],
          fields: [
            %FieldDefinition{
              name: :target,
              type: :string,
              description: nil,
              opts: []
            },
            %FieldDefinition{
              name: :count,
              type: :integer,
              description: "Counter",
              opts: [optional: true, some_tag: 1]
            }
          ]
        }
      ]

      assert MyApp.DocumentedPacketHandler.__defs__() == expected
    end
  end

  ## Extension

  describe "defextension/1" do
    test "can delegate ignored packets" do
      assert MyApp.ExtendedPacketHandler.handle_packet("USELESS", %{}, 123) == {:cont, 123}
    end

    test "can delegate packets without fields" do
      assert MyApp.ExtendedPacketHandler.handle_packet("PING", %{}, 123) ==
               {:ping, "PING", %{}, 123}
    end

    test "can delegate packets with fields" do
      args = %{username: "user", password: "password"}
      assert MyApp.ExtendedPacketHandler.handle_packet("LOGIN", args, 123) == :connect
    end

    test "can delegate packets packets with options" do
      args = %{username: "admin", password: "password"}
      assert MyApp.ExtendedPacketHandler.handle_packet("LOGIN", args, 123) == :connect_admin
    end

    test "returns valid definitions" do
      expected = [
        %PacketDefinition{
          header: "USELESS",
          description: "Packet ignored",
          tags: [:ignored],
          fields: []
        },
        %PacketDefinition{
          header: "PING",
          description: "Simple ping",
          tags: [],
          fields: [
            %FieldDefinition{
              name: :target,
              type: :string,
              description: nil,
              opts: []
            },
            %FieldDefinition{
              name: :count,
              type: :integer,
              description: "Counter",
              opts: [optional: true, some_tag: 1]
            }
          ]
        }
      ]

      assert MyApp.ExtendedDocPacketHandler.__defs__() == expected
    end
  end

  ## TODO: Test documentation with custom types

  ## Helpers

  defp simple_handle_packet(header, args, socket) do
    MyApp.SimplePacketHandler.handle_packet(header, args, socket)
  end
end
