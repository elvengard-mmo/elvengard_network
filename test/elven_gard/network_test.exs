defmodule ElvenGard.NetworkTest do
  use ExUnit.Case
  doctest ElvenGard.Network

  test "greets the world" do
    assert ElvenGard.Network.hello() == :world
  end
end
