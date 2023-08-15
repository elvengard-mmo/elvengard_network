defmodule ElvenGard.Network.NetworkCodec do
  @moduledoc ~S"""
  Behavior for Defining Packet Encoding and Decoding

  Implementations of this behavior are responsible for parsing incoming 
  raw binary data into structured packets and encoding structured packets into
  binary data for transmission over the network.

  For more information on how to implement this behavior and use packet encoding
  and decoding, please refer to the [NetworkCodec guide]
  (<NETWORKCODEC_URL>).
  """

  alias ElvenGard.Network.Socket

  @doc """
  Identifies and extracts the first packet from raw binary data.

  This function searches for the first packet within the raw binary data and
  returns it along with the remaining binary data. The extracted packet will
  be passed to the `decode/2` callback for further processing.

  ## Examples

      raw = <<packet1::binary, remaining::binary>>
      {packet1, remaining} = ElvenGard.Network.NetworkCodec.next(raw, socket)

  """
  @callback next(raw :: bitstring, socket :: Socket.t()) ::
              {packet_raw :: bitstring, remaining :: bitstring}

  @doc """
  Decodes a packet from raw binary data.

  This callback function is responsible for decoding a packet from the provided
  raw binary data. The decoded packet should be returned as a struct containing
  the parsed fields.

  ## Examples

      raw_packet = <<1::8, 123::8, "some data"::binary>>  # Example raw packet data
      %MessageStruct{id: 123, data: "some data"} = ElvenGard.Network.NetworkCodec.decode(raw_packet, socket)

  """
  @callback decode(raw :: bitstring, socket :: Socket.t()) :: struct

  @doc """
  Encodes a packet for transmission.

  This callback function is responsible for encoding a structured packet or raw
  binary data into the binary format suitable for transmission over the network.
  The resulting encoded data, in the form of `iodata()`, can be directly sent
  over the network.

  ## Examples

      packet = %MessageStruct{id: 1, data: "Hello"}
      <<header::8, id::8, data::binary>> = ElvenGard.Network.NetworkCodec.encode(packet, socket)

  """
  @callback encode(packet | raw, socket :: Socket.t()) :: iodata()
            when packet: struct(), raw: iodata()
end
