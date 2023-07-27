defmodule ElvenGard.Network.DummyEncoder do
  @moduledoc false

  @behaviour ElvenGard.Network.NetworkCodec

  @impl true
  def next(_raw), do: raise("unimplemented")

  @impl true
  def deserialize(_raw, _socket), do: raise("unimplemented")

  @impl true
  def serialize(raw, _socket), do: raw
end
