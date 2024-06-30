defmodule ElvenGard.Network.EndpointTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint

  ## Test

  describe "child_spec/1" do
    test "returns a valid child_spec" do
      assert %{id: _, start: _} = Endpoint.child_spec(port: 1234, socket_handler: __MODULE__)
    end

    test "require :port option" do
      assert_raise ArgumentError, ":port option is required", fn ->
        Endpoint.child_spec(socket_handler: __MODULE__)
      end
    end

    test "require :socket_handler option" do
      assert_raise ArgumentError, ":socket_handler option is required", fn ->
        Endpoint.child_spec(port: 1234)
      end
    end
  end
end
