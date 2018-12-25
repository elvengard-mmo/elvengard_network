defmodule LoginServerTest do
  use ExUnit.Case
  doctest LoginServer

  test "greets the world" do
    assert LoginServer.hello() == :world
  end
end
