defmodule ElvenGard.Protocol.Textual.FloatTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Textual.FloatType

  test "Encode textual float type" do
    got = FloatType.encode(13.37)
    expected = "13.37"

    assert got == expected
  end

  test "Decode textual float type" do
    got = FloatType.decode("13.37")
    expected = 13.37

    assert got == expected
  end
end
