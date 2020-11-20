defmodule ElvenGard.Protocol.Binary.StringTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Binary.StringType

  describe "Encode binary string type:" do
    test "basic behaviour" do
      got = StringType.encode("first test")
      expected = "first test"

      assert got == expected
    end
  end

  describe "Decode binary string type:" do
    test "raise without length" do
      assert_raise ArgumentError, fn ->
        StringType.decode("no length")
      end
    end

    test "without rest (size in bytes)" do
      msg = "no rest"
      len = byte_size(msg)
      got = StringType.decode(msg, bytes: len)
      expected = {msg, <<>>}

      assert got == expected
    end

    test "without rest (size in bits)" do
      msg = "no rest"
      len = byte_size(msg) * 8
      got = StringType.decode(msg, bits: len)
      expected = {msg, <<>>}

      assert got == expected
    end

    test "with rest (size in bytes)" do
      msg = "with rest"
      rest = <<0x13::size(8), 0x37::size(8)>>
      len = byte_size(msg)
      full_msg = <<msg::binary, rest::binary>>
      got = StringType.decode(full_msg, bytes: len)
      expected = {msg, rest}

      assert got == expected
    end

    test "with rest (size in bits)" do
      msg = "with rest"
      rest = <<0x13::size(8), 0x37::size(8)>>
      len = byte_size(msg) * 8
      full_msg = <<msg::binary, rest::binary>>
      got = StringType.decode(full_msg, bits: len)
      expected = {msg, rest}

      assert got == expected
    end
  end
end
