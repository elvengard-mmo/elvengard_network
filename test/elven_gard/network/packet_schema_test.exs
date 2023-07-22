defmodule ElvenGard.Network.PacketSchemaTest do
  use ExUnit.Case, async: true

  defmodule SimplePackets do
    use ElvenGard.Network.PacketSchema
    use ExUnit.Case, async: true

    alias ElvenGard.Network.CustomTypes.{Boolean, Date, Int, Str}
    alias ElvenGard.Network.Socket

    packet "my_simple_packet" do
      field :id, Int
      field :name, Str
      field :enabled, Boolean
      field :created_at, Date
    end

    test "schemas list" do
      metadata = [
        %{
          id: "my_simple_packet",
          name: MySimplePacket,
          guards: nil,
          fields: [
            %{name: :id, type: ElvenGard.Network.CustomTypes.Int, opts: []},
            %{name: :name, type: ElvenGard.Network.CustomTypes.Str, opts: []},
            %{name: :enabled, type: ElvenGard.Network.CustomTypes.Boolean, opts: []},
            %{name: :created_at, type: ElvenGard.Network.CustomTypes.Date, opts: []}
          ]
        }
      ]

      assert SimplePackets.__schemas__() == metadata
    end

    test "create a structure" do
      assert :code.module_status(MySimplePacket) == :loaded
      assert function_exported?(MySimplePacket, :__struct__, 1)
    end

    test "introspection" do
      assert MySimplePacket.__schema__(:id) == "my_simple_packet"
      assert MySimplePacket.__schema__(:name) == MySimplePacket
      assert MySimplePacket.__schema__(:guards) == nil

      assert MySimplePacket.__schema__(:fields) == [
               %{name: :id, type: ElvenGard.Network.CustomTypes.Int, opts: []},
               %{name: :name, type: ElvenGard.Network.CustomTypes.Str, opts: []},
               %{name: :enabled, type: ElvenGard.Network.CustomTypes.Boolean, opts: []},
               %{name: :created_at, type: ElvenGard.Network.CustomTypes.Date, opts: []}
             ]
    end

    describe "decode/3" do
      test "is defined" do
        assert function_exported?(SimplePackets, :decode, 3)
      end

      test "parse a binary and returns a structure" do
        packet = SimplePackets.decode("my_simple_packet", "1337 Admin 1 2023-07-21", %Socket{})

        assert packet.__struct__ == MySimplePacket
        assert packet.id == 1337
        assert packet.name == "Admin"
        assert packet.enabled == true
        assert packet.created_at == ~D[2023-07-21]
      end
    end
  end

  # defmodule ComplexPackets do
  #   use ElvenGard.Network.PacketSchema
  #   use ExUnit.Case, async: true

  #   alias ElvenGard.Network.CustomTypes.{Int, Str}
  #   alias ElvenGard.Network.Socket

  #   packet "my_simple_packet" do
  #     field :id, Int
  #     field :name, Str, fill: true
  #   end
  # end
end
