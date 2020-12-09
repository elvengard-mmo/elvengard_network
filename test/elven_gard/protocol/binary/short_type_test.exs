defmodule ElvenGard.Protocol.Binary.ShortTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Binary.ShortType

  # describe "Encode binary short type:" do
  #   @tag :skip
  #   test "default behaviour (unsigned + big)" do
  #     got = ShortType.encode(0x1337)
  #     expected = <<0x1337::size(16)>>

  #     assert got == expected
  #   end

  #   @tag :skip
  #   test "default behaviour with overflow (unsigned + big)" do
  #     got = ShortType.encode(0x901337)
  #     expected = <<0x1337::size(16)>>

  #     assert got == expected
  #   end

  #   test "signed + little" do
  #     got = ShortType.encode(0x1337, signed: true, endian: :little)
  #     expected = <<0x1337::signed-little-size(16)>>

  #     assert got == expected
  #   end

  #   test "unsigned + little" do
  #     got = ShortType.encode(0x1337, signed: false, endian: :little)
  #     expected = <<0x1337::unsigned-little-size(16)>>

  #     assert got == expected
  #   end

  #   @tag :skip
  #   test "signed + big" do
  #     got = ShortType.encode(0x1337, signed: true, endian: :big)
  #     expected = <<0x1337::signed-big-size(16)>>

  #     assert got == expected
  #   end

  #   @tag :skip
  #   test "unsigned + big" do
  #     got = ShortType.encode(0x1337, signed: false, endian: :big)
  #     expected = <<0x1337::unsigned-big-size(16)>>

  #     assert got == expected
  #   end

  #   test "signed + native" do
  #     got = ShortType.encode(0x1337, signed: true, endian: :native)
  #     expected = <<0x1337::signed-native-size(16)>>

  #     assert got == expected
  #   end

  #   test "unsigned + native" do
  #     got = ShortType.encode(0x1337, signed: false, endian: :native)
  #     expected = <<0x1337::unsigned-native-size(16)>>

  #     assert got == expected
  #   end
  # end

  describe "Decode binary short type:" do
    @tag :skip
    test "default behaviour without rest (unsigned + big)" do
      got = ShortType.decode(<<0x1337::size(16)>>)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    @tag :skip
    test "default behaviour with rest (unsigned + big)" do
      got = ShortType.decode(<<0x1337::size(16), 0x42::signed-size(16), 0x01::little-size(16)>>)
      expected = {0x1337, <<0x42::signed-size(16), 0x01::little-size(16)>>}

      assert got == expected
    end

    test "signed + little" do
      got = ShortType.decode(<<0x1337::signed-little-size(16)>>, signed: true, endian: :little)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    test "unsigned + little" do
      got = ShortType.decode(<<0x1337::unsigned-little-size(16)>>, signed: false, endian: :little)

      expected = {0x1337, <<>>}

      assert got == expected
    end

    @tag :skip
    test "signed + big" do
      got = ShortType.decode(<<0x1337::signed-big-size(16)>>, signed: true, endian: :big)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    @tag :skip
    test "unsigned + big" do
      got = ShortType.decode(<<0x1337::unsigned-big-size(16)>>, signed: false, endian: :big)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    test "signed + native" do
      got = ShortType.decode(<<0x1337::signed-native-size(16)>>, signed: true, endian: :native)
      expected = {0x1337, <<>>}

      assert got == expected
    end

    test "unsigned + native" do
      got = ShortType.decode(<<0x1337::unsigned-native-size(16)>>, signed: false, endian: :native)

      expected = {0x1337, <<>>}

      assert got == expected
    end
  end
end
