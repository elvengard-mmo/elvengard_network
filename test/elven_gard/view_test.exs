Code.require_file("../fixtures/views.exs", __DIR__)

defmodule ElvenGard.ViewTest do
  use ExUnit.Case, async: true

  describe "render/2" do
    test "returns the view corresponding to the given name" do
      assert MyApp.SimpleView.render(:ping, nil) == %MyApp.PingPacket{}
      assert MyApp.SimpleView.render(:hello, %{msg: "WORLD"}) == %MyApp.HelloPacket{msg: "WORLD"}
    end

    test "raises an error with an unknown name" do
      assert_raise ElvenGard.UnknownViewError, fn ->
        MyApp.SimpleView.render(:unknown_name, nil)
      end
    end
  end
end
