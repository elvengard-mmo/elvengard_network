defmodule ElvenGard.Network.Endpoint.PacketCodec do
  @moduledoc ~S"""
  Define a behaviour for packet encoding and decoding
  """

  alias ElvenGard.Network.Socket

  @doc """
  Returns the first packet found in a raw binary

  The result will be sent to the `c:deserialize/2` callback
  """
  @callback next(raw :: bitstring) :: {packet_raw :: bitstring, remaining :: bitstring}

  @doc "Deserializes a packet"
  @callback deserialize(raw :: bitstring, socket :: Socket.t()) :: map | struct

  @doc "Serialize a `ElvenGard.Network.View`"
  @callback serialize(raw :: iodata, socket :: Socket.t()) :: iodata
end
