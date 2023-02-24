defmodule ElvenGard.Protocol.Binary.PaddingTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Binary.PaddingType

  describe "decode/2" do
    test "raises without option" do
      assert_raise ArgumentError, ~r/must specify a size/, fn ->
        PaddingType.decode("foo", [])
      end
    end

    test "returns all the data with fill option" do
      assert PaddingType.decode("foo", fill: true) == {"foo", ""}
    end

    test "returns the corresponding length in bits" do
      assert PaddingType.decode("foobar", bits: 24) == {"foo", "bar"}
    end

    test "returns the corresponding length in bytes" do
      assert PaddingType.decode("foobaz", bytes: 3) == {"foo", "baz"}
    end
  end
end
