defmodule ElvenGard.ViewTest do
  use ExUnit.Case

  defmodule BasicView do
    use ElvenGard.View

    @impl ElvenGard.View
    def render(:some_key, _args), do: :ok
  end

  describe "Basic `View` behaviour:" do
    test "render/1 with an existing key" do
      assert BasicView.render(:some_key, nil) == :ok
    end

    test "render/1 with an unknown key" do
      assert_raise ElvenGard.UnknownViewError, fn ->
        BasicView.render(:unknown_key, nil)
      end
    end
  end
end
