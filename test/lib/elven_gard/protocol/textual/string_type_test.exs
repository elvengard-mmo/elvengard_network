defmodule ElvenGard.Protocol.Textual.StringTypeTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Protocol.Textual.StringType

  test "Encode textual string type" do
    expected = "Some string"
    got = StringType.encode(expected)

    assert got == expected
  end

  test "Decode textual string type" do
    expected = "Another string"
    got = StringType.decode(expected)

    assert got == expected
  end
end
