defmodule ElvenGard.Protocol.Textual.StringTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Textual.StringType

  # describe "encode/2" do
  #   test "returns an encoded string" do
  #     expected = "Some string"
  #     got = StringType.encode("Some string", [])
  #     assert got == expected
  #   end

  #   test "must raise if the value is not a string" do
  #     assert_raise ArgumentError, ~r"must be a string", fn ->
  #       StringType.encode(123, [])
  #     end
  #   end
  # end

  describe "decode/2" do
    test "without option" do
      expected = {"This is a string", ""}
      got = StringType.decode("This is a string", [])
      assert got == expected
    end

    test "with separator set to `nil`" do
      expected = {"This is a string", ""}
      got = StringType.decode("This is a string", separator: nil)
      assert got == expected
    end

    test "with delimitor in string" do
      expected = {"This", "is a string"}
      got = StringType.decode("This is a string", separator: " ")
      assert got == expected
    end

    test "with delimitor not in string" do
      expected = {"This is a string", ""}
      got = StringType.decode("This is a string", separator: ".")
      assert got == expected
    end

    test "with invalid value" do
      needle = ~r"value to decode must be a string"

      assert_raise ArgumentError, needle, fn ->
        StringType.decode(123, [])
      end
    end

    test "with invalid separator" do
      needle = ~r"separator must be a string"

      assert_raise ArgumentError, needle, fn ->
        StringType.decode("", separator: 123)
      end
    end
  end
end
