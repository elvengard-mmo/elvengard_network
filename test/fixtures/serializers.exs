defmodule MyApp.LFSerializer do
  @behaviour ElvenGard.Socket.Serializer

  @impl true
  def aliases(), do: []

  @impl true
  def encode!(data, _socket), do: data <> "\n"

  @impl true
  def decode!(data, _socket), do: String.split(data, " ", part: 2)
end
