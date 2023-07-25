defmodule MyApp.View do
  use ElvenGard.Network.View

  @impl true
  def render(:ping, _), do: :pong
  def render(:greet, %{arg: arg}), do: {:greet, arg}
end
