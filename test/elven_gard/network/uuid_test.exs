defmodule ElvenGard.Network.UUIDTest do
  use ExUnit.Case, async: true

  test "returns a random UUID as a string" do
    uuid1 = ElvenGard.Network.UUID.uuid4()

    assert is_binary(uuid1)
    assert ElvenGard.Network.UUID.uuid4() != uuid1
  end
end
