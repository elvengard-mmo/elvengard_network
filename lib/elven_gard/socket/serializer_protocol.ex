defprotocol ElvenGard.Socket.SerializerProtocol do
  @doc "Define how a type should be serialized"
  def serialize(data, opts)
end
