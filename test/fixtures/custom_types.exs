defmodule MyApp.BasicType do
  use ElvenGard.Type

  @impl true
  def encode(val, opts), do: {val, opts}

  @impl true
  def decode(val, opts), do: {val, opts}
end
