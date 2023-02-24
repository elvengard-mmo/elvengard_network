defmodule MyApp.BasicType do
  use ElvenGard.FieldType

  # @impl true
  # def encode(val, opts), do: {val, opts}

  @impl true
  def decode(val, opts), do: {val, opts}
end
