defmodule ElvenGard.ViewTest do
  use ExUnit.Case, async: true

  defmodule BasicView do
    use ElvenGard.View

    @impl ElvenGard.View
    def render(:some_key, _args), do: :ok
  end

  describe "render/2" do
    test "returns a value with an existing key" do
      assert BasicView.render(:some_key, nil) == :ok
    end

    test "raises an error with an unknown key" do
      assert_raise ElvenGard.UnknownViewError, fn ->
        BasicView.render(:unknown_key, nil)
      end
    end
  end
end
