defmodule MyApp.BasicSerializer do
  use ElvenGard.Socket.Serializer

  @impl true
  def aliases(), do: [foo: MyApp.Bar, abc: MyApp.Baz]

  @impl true
  def encode!(data, _opts), do: data

  @impl true
  def decode!(data, _assigns), do: data
end

defmodule MyApp.LFSerializer do
  @behaviour ElvenGard.Socket.Serializer

  @impl true
  def aliases(), do: []

  @impl true
  def encode!(data, _opts), do: [data, "\n"]

  @impl true
  def decode!(data, _assigns), do: {"HEADER", data}
end

defmodule MyApp.SimpleTextSerializer do
  use ElvenGard.Socket.TextualSerializer, separator: " "

  ## Callbacks

  # @impl true
  # def handle_decode(data, _opts), do: data

  ## SerializerProtocol implementations

  defimpl ElvenGard.Socket.SerializerProtocol, for: BitString do
    def serialize(data, _opts), do: data
  end

  defimpl ElvenGard.Socket.SerializerProtocol, for: Atom do
    def serialize(data, _opts), do: Atom.to_string(data)
  end

  defimpl ElvenGard.Socket.SerializerProtocol, for: Integer do
    def serialize(data, _opts), do: Integer.to_string(data)
  end
end
