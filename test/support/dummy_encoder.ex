defmodule ElvenGard.Network.DummyEncoder do
  @moduledoc false

  @behaviour ElvenGard.Network.NetworkCodec

  @impl true
  def next(_raw, _socket), do: raise("unimplemented")

  @impl true
  def decode(_raw, _socket), do: raise("unimplemented")

  @impl true
  def encode(raw, _socket), do: raw
end
