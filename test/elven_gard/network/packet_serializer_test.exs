defmodule ElvenGard.Network.PacketSerializerTest do
  use ExUnit.Case, async: true

  defmodule SimplePackets do
    use ElvenGard.Network.PacketSerializer
    use ExUnit.Case, async: true

    alias ElvenGard.Network.CustomTypes.{Boolean, Date, Int, Str}
    alias ElvenGard.Network.Socket

    ## Packets def

    @deserializable true
    defpacket "my_simple_packet" do
      field :id, Int
      field :name, Str
      field :enabled, Boolean
      field :created_at, Date
    end

    ## Tests

    test "schemas list" do
      metadata = [
        %{
          id: "my_simple_packet",
          name: MySimplePacket,
          parent: ElvenGard.Network.PacketSerializerTest.SimplePackets,
          mod: ElvenGard.Network.PacketSerializerTest.SimplePackets.MySimplePacket,
          serializable: false,
          deserializable: true,
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
      assert :code.module_status(__MODULE__.MySimplePacket) == :loaded
      assert function_exported?(__MODULE__.MySimplePacket, :__struct__, 1)
    end

    test "introspection" do
      assert __MODULE__.MySimplePacket.__schema__(:id) == "my_simple_packet"
      assert __MODULE__.MySimplePacket.__schema__(:name) == MySimplePacket
      assert __MODULE__.MySimplePacket.__schema__(:guards) == nil

      assert __MODULE__.MySimplePacket.__schema__(:fields) == [
               %{name: :id, type: ElvenGard.Network.CustomTypes.Int, opts: []},
               %{name: :name, type: ElvenGard.Network.CustomTypes.Str, opts: []},
               %{name: :enabled, type: ElvenGard.Network.CustomTypes.Boolean, opts: []},
               %{name: :created_at, type: ElvenGard.Network.CustomTypes.Date, opts: []}
             ]
    end

    describe "deserialize/3" do
      test "is defined" do
        assert function_exported?(SimplePackets, :deserialize, 3)
      end

      test "parse a binary and returns a structure" do
        packet =
          SimplePackets.deserialize("my_simple_packet", "1337 Admin 1 2023-07-21", %Socket{})

        assert packet.__struct__ == __MODULE__.MySimplePacket
        assert packet.id == 1337
        assert packet.name == "Admin"
        assert packet.enabled == true
        assert packet.created_at == ~D[2023-07-21]
      end

      test "raise an error if remaining bytes" do
        assert_raise RuntimeError, ~r/remaining bytes for /, fn ->
          SimplePackets.deserialize("my_simple_packet", "1337 Admin 1 2023-07-21 foo", %Socket{})
        end
      end
    end
  end

  defmodule StringPackets do
    use ElvenGard.Network.PacketSerializer
    use ExUnit.Case, async: true

    alias ElvenGard.Network.CustomTypes.{Boolean, Str}
    alias ElvenGard.Network.Socket

    ## Packets def

    @deserializable true
    defpacket "no_field"

    @deserializable true
    defpacket "no_field_but_guard" when socket.assigns.state == :foo

    @deserializable true
    defpacket "no_field_but_name", as: NoFieldButName2

    @deserializable true
    defpacket "no_field_but_name_and_guard" when socket.assigns.state == :foo,
      as: NoFieldButNameAndGuard2

    @deserializable true
    defpacket "no_field_but_name_and_guard" when socket.assigns.state == :bar,
      as: NoFieldButNameAndGuard3

    @deserializable true
    defpacket "with_empty_fields", do: :ok

    @deserializable true
    defpacket "with_guards" when socket.assigns.state == :foo do
      field :value, Str
    end

    @deserializable true
    defpacket "with_name", as: WithName2 do
      field :value, Str
    end

    @deserializable true
    defpacket "with_guards_and_name" when socket.assigns.state == :foo, as: WithGuardsAndName2 do
      field :value, Str
    end

    @deserializable true
    defpacket "with_options" do
      field :value, Str, fill: true
    end

    @deserializable true
    defpacket "with_condition" do
      field :enabled, Boolean
      field :value, Str, if: packet.enabled
    end

    ## Tests

    describe "deserialize packet with" do
      test "no field" do
        assert packet = deserialize("no_field")
        assert packet.__struct__ == __MODULE__.NoField
      end

      test "no field but guards" do
        assert packet = deserialize("no_field_but_guard", state: :foo)
        assert packet.__struct__ == __MODULE__.NoFieldButGuard
        assert_raise FunctionClauseError, fn -> deserialize("no_field_but_guard", state: :bar) end
      end

      test "no field but name" do
        assert packet = deserialize("no_field_but_name")
        assert packet.__struct__ == __MODULE__.NoFieldButName2
      end

      test "no field but name and guard" do
        assert packet = deserialize("no_field_but_name_and_guard", state: :foo)
        assert packet.__struct__ == __MODULE__.NoFieldButNameAndGuard2

        assert packet = deserialize("no_field_but_name_and_guard", state: :bar)
        assert packet.__struct__ == __MODULE__.NoFieldButNameAndGuard3
      end

      test "empty field" do
        assert packet = deserialize("with_empty_fields")
        assert packet.__struct__ == __MODULE__.WithEmptyFields
      end

      test "guards" do
        assert packet = deserialize("with_guards", raw: "bar", state: :foo)
        assert packet.__struct__ == __MODULE__.WithGuards
        assert packet.value == "bar"
      end

      test "name" do
        assert packet = deserialize("with_name", raw: "bar")
        assert packet.__struct__ == __MODULE__.WithName2
        assert packet.value == "bar"
      end

      test "guards and name" do
        assert packet = deserialize("with_guards_and_name", raw: "bar", state: :foo)
        assert packet.__struct__ == __MODULE__.WithGuardsAndName2
        assert packet.value == "bar"
      end

      test "options" do
        assert packet = deserialize("with_options", raw: "foo bar")
        assert packet.__struct__ == __MODULE__.WithOptions
        assert packet.value == "foo bar"
      end

      test "condition" do
        assert packet = deserialize("with_condition", raw: "0")
        assert packet.__struct__ == __MODULE__.WithCondition
        assert packet.enabled == false
        assert packet.value == nil

        assert packet = deserialize("with_condition", raw: "1 foobar")
        assert packet.__struct__ == __MODULE__.WithCondition
        assert packet.enabled == true
        assert packet.value == "foobar"
      end
    end

    ## Helpers

    defp deserialize(name, opts \\ []) do
      assigns = if opts[:state], do: %{state: opts[:state]}, else: nil
      StringPackets.deserialize(name, opts[:raw] || "", %Socket{assigns: assigns})
    end
  end

  defmodule BinaryPackets do
    use ExUnit.Case, async: true

    ## Tests

    test "no field but name" do
      assert_raise RuntimeError, ~r/a module name is required for integer packet ids/, fn ->
        Code.compile_string("""
        defmodule CantCompile do
          use ElvenGard.Network.PacketSerializer

          @deserializable true
          defpacket 0x00, do: :ok
        end
        """)
      end
    end
  end
end
