defmodule MyApp.PingView do
  defstruct []

  defimpl ElvenGard.Socket.SerializerProtocol do
    def serialize(_data, _opts), do: "PING"
  end
end

defmodule MyApp.HelloView do
  defstruct [:msg]

  import ElvenGard.Socket.Serializer, only: [serialize: 1]

  defimpl ElvenGard.Socket.SerializerProtocol do
    def serialize(data, separator: sep) do
      ["HELLO", sep, serialize(data.msg)]
    end
  end
end
