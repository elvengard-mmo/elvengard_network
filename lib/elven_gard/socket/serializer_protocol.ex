defprotocol ElvenGard.Socket.SerializerProtocol do
  @doc "Define how a type should be serialized"
  @spec serialize(any(), keyword()) :: iodata()
  def serialize(data, opts)
end
