defmodule ElvenGard.Protocol.Binary.ByteTypeTest do
  use ExUnit.Case

  alias ElvenGard.Protocol.Binary.ByteType

  describe "Encode binary byte type:" do
    test "basic behaviour" do
      got = ByteType.encode(0x13)
      expected = <<0x13>>

      assert got == expected
    end

    test "with number overflow" do
      got = ByteType.encode(0x1337, [])
      expected = <<0x37>>

      assert got == expected
    end
  end

  describe "Decode binary byte type:" do
    test "without rest" do
      got = ByteType.decode(<<0x13>>)
      expected = {0x13, <<>>}

      assert got == expected
    end

    test "with rest" do
      got = ByteType.decode(<<0x13, 0x37, 0x00>>, [])
      expected = {0x13, <<0x37, 0x00>>}

      assert got == expected
    end
  end
end
