defmodule ElvenGard.Protocol.Textual.StringTypeTest do
  use ExUnit.Case

  alias ElvenGard.Protocol.Textual.StringType

  test "Encode textual string type" do
    got = StringType.encode("1337 1337 1337")
    expected = "1337 1337 1337"

    assert expected == got
  end

  test "Decode textual string type" do
    got = StringType.decode("1337 1337 1337")
    expected = "1337 1337 1337"

    assert expected == got
  end
end
