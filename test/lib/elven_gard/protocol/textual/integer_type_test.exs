defmodule ElvenGard.Protocol.Textual.IntegerTypeTest do
  use ExUnit.Case

  alias ElvenGard.Protocol.Textual.IntegerType

  test "Encode textual integer type" do
    got = IntegerType.encode(1337)
    expected = "1337"

    assert expected == got
  end

  test "Decode textual integer type" do
    got = IntegerType.decode("1337")
    expected = 1337

    assert expected == got
  end
end
