defmodule ElvenGard.Protocol.Textual.IntegerTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Textual.IntegerType

  test "Encode textual integer type" do
    got = IntegerType.encode(1337)
    expected = "1337"

    assert got == expected
  end

  test "Decode textual integer type" do
    got = IntegerType.decode("1337")
    expected = 1337

    assert got == expected
  end
end
