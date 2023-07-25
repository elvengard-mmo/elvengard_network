Code.require_file("../../fixtures/views.exs", __DIR__)

defmodule ElvenGard.Network.ViewTest do
  use ExUnit.Case, async: true

  describe "render/2" do
    test "returns the view corresponding to the given name" do
      assert MyApp.View.render(:ping, nil) == :pong
      assert MyApp.View.render(:greet, %{arg: "Hi!"}) == {:greet, "Hi!"}
    end

    test "raises an error with an unknown name" do
      assert_raise ElvenGard.Network.UnknownViewError, fn ->
        MyApp.View.render(:unknown, nil)
      end
    end
  end
end
