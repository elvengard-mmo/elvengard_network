defmodule ElvenGard.Protocol.Textual.IntegerTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Textual.IntegerType

  # describe "encode/2" do
  #   test "returns an encoded string" do
  #     expected = "123"
  #     got = IntegerType.encode(123, [])
  #     assert got == expected
  #   end

  #   test "must raise if the value is not an integer" do
  #     assert_raise ArgumentError, ~r"must be an integer", fn ->
  #       IntegerType.encode("123", [])
  #     end
  #   end
  # end

  describe "decode/2" do
    test "without option" do
      expected = {123, ""}
      got = IntegerType.decode("123", [])
      assert got == expected
    end

    test "with separator set to `nil`" do
      expected = {123, ""}
      got = IntegerType.decode("123", separator: nil)
      assert got == expected
    end

    test "with delimitor in string" do
      expected = {123, "is an integer"}
      got = IntegerType.decode("123 is an integer", separator: " ")
      assert got == expected
    end

    test "with delimitor not in string" do
      expected = {123, ""}
      got = IntegerType.decode("123", separator: ".")
      assert got == expected
    end

    test "with invalid value" do
      needle = ~r"value to decode must be a string"
      # Doesn't contains the previous error (must be the Elixir error for String.to_integer/1)
      needle2 = ~r"^((?!value to decode must be a string).)*$"

      assert_raise ArgumentError, needle, fn ->
        IntegerType.decode(123, [])
      end

      assert_raise ArgumentError, needle2, fn ->
        IntegerType.decode("123 is an integer", [])
      end
    end

    test "with invalid separator" do
      needle = ~r"separator must be a string"

      assert_raise ArgumentError, needle, fn ->
        IntegerType.decode("", separator: 123)
      end
    end
  end
end
