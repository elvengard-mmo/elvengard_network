defmodule ElvenGard.Protocol.Binary.LongTypeTest do
  use ExUnit.Case

  alias ElvenGard.Protocol.Binary.LongType

  describe "Encode binary long type:" do
    @tag :pending
    test "default behaviour (unsigned + big)" do
      got = LongType.encode(0x1337)
      expected = <<0x1337::size(64)>>

      assert got == expected
    end

    @tag :pending
    test "default behaviour with overflow (unsigned + big)" do
      got = LongType.encode(0x909090901337133713371337)
      expected = <<0x1337133713371337::size(64)>>

      assert got == expected
    end

    test "signed + little" do
      got = LongType.encode(0x1337, signed: true, endian: :little)
      expected = <<0x1337::signed-little-size(64)>>

      assert got == expected
    end

    test "unsigned + little" do
      got = LongType.encode(0x1337, signed: false, endian: :little)
      expected = <<0x1337::unsigned-little-size(64)>>

      assert got == expected
    end

    @tag :pending
    test "signed + big" do
      got = LongType.encode(0x1337, signed: true, endian: :big)
      expected = <<0x1337::signed-big-size(64)>>

      assert got == expected
    end

    @tag :pending
    test "unsigned + big" do
      got = LongType.encode(0x1337, signed: false, endian: :big)
      expected = <<0x1337::unsigned-big-size(64)>>

      assert got == expected
    end

    test "signed + native" do
      got = LongType.encode(0x1337, signed: true, endian: :native)
      expected = <<0x1337::signed-native-size(64)>>

      assert got == expected
    end

    test "unsigned + native" do
      got = LongType.encode(0x1337, signed: false, endian: :native)
      expected = <<0x1337::unsigned-native-size(64)>>

      assert got == expected
    end
  end

  describe "Decode binary long type:" do
    @tag :pending
    test "default behaviour without rest (unsigned + big)" do
      got = LongType.decode(<<0x1337::size(64)>>)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    @tag :pending
    test "default behaviour with rest (unsigned + big)" do
      got = LongType.decode(<<0x1337::size(64), 0x42::signed-size(64), 0x01::little-size(64)>>)
      expected = {0x1337, <<0x42::signed-size(64), 0x01::little-size(64)>>}

      assert got == expected
    end

    test "signed + little" do
      got = LongType.decode(<<0x1337::signed-little-size(64)>>, signed: true, endian: :little)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    test "unsigned + little" do
      got = LongType.decode(<<0x1337::unsigned-little-size(64)>>, signed: false, endian: :little)

      expected = {0x1337, <<>>}

      assert got == expected
    end

    @tag :pending
    test "signed + big" do
      got = LongType.decode(<<0x1337::signed-big-size(64)>>, signed: true, endian: :big)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    @tag :pending
    test "unsigned + big" do
      got = LongType.decode(<<0x1337::unsigned-big-size(64)>>, signed: false, endian: :big)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    test "signed + native" do
      got = LongType.decode(<<0x1337::signed-native-size(64)>>, signed: true, endian: :native)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    test "unsigned + native" do
      got = LongType.decode(<<0x1337::unsigned-native-size(64)>>, signed: false, endian: :native)

      expected = {0x1337, <<>>}

      assert got == expected
    end
  end
end
