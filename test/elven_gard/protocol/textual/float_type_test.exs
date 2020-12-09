defmodule ElvenGard.Protocol.Textual.FloatTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Textual.FloatType

  # describe "encode/2" do
  #   test "returns an encoded string" do
  #     expected = "13.37"
  #     got = FloatType.encode(13.37, [])
  #     assert got == expected
  #   end

  #   test "must raise if the value is not a float" do
  #     assert_raise ArgumentError, ~r"must be a float", fn ->
  #       FloatType.encode("123", [])
  #     end
  #   end
  # end

  describe "decode/2" do
    test "without option" do
      expected = {13.37, ""}
      got = FloatType.decode("13.37", [])
      assert got == expected
    end

    test "with separator set to `nil`" do
      expected = {13.37, ""}
      got = FloatType.decode("13.37", separator: nil)
      assert got == expected
    end

    test "with delimitor in string" do
      expected = {13.37, "is a float"}
      got = FloatType.decode("13.37 is a float", separator: " ")
      assert got == expected
    end

    test "with delimitor not in string" do
      expected = {13.37, ""}
      got = FloatType.decode("13.37", separator: ";")
      assert got == expected
    end

    test "with invalid value" do
      needle = ~r"value to decode must be a string"
      # Doesn't contains the previous error (must be the Elixir error for String.to_float/1)
      needle2 = ~r"^((?!value to decode must be a string).)*$"

      assert_raise ArgumentError, needle, fn ->
        FloatType.decode(123, [])
      end

      assert_raise ArgumentError, needle2, fn ->
        FloatType.decode("123 is an integer", [])
      end
    end

    test "with invalid separator" do
      needle = ~r"separator must be a string"

      assert_raise ArgumentError, needle, fn ->
        FloatType.decode("", separator: 123)
      end
    end
  end
end
