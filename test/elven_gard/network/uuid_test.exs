defmodule ElvenGard.Network.UUIDTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.UUID

  test "returns a random UUID as a string" do
    uuid1 = UUID.uuid4()

    assert is_binary(uuid1)
    assert UUID.uuid4() != uuid1
  end
end
