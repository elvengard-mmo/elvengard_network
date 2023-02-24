defmodule ElvenGard.Protocol.Binary.ByteTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Binary.ByteType

  # describe "encode/2" do
  #   test "can encode an integer" do
  #     got = ByteType.encode(0x13, [])
  #     expected = <<0x13>>

  #     assert got == expected
  #   end

  #   test "supports integer overflow" do
  #     got = ByteType.encode(0x1337, [])
  #     expected = <<0x37>>

  #     assert got == expected
  #   end
  # end

  describe "decode/2" do
    test "can decode an integer" do
      got = ByteType.decode(<<0x13>>, [])
      expected = {0x13, <<>>}

      assert got == expected
    end

    test "returns the rest" do
      got = ByteType.decode(<<0x13, 0x37, 0x00>>, [])
      expected = {0x13, <<0x37, 0x00>>}

      assert got == expected
    end
  end
end
