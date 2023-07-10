defmodule MinecraftEx.Endpoint.PacketCodec do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.PacketCodec
  """

  @behaviour ElvenGard.Network.Endpoint.PacketCodec

  alias MinecraftEx.Types.VarInt
  alias MinecraftEx.Endpoint.PacketSchemas

  @impl true
  def next(<<>>), do: {nil, <<>>}

  def next(message) do
    {length, rest} = VarInt.decode(message, [])

    case byte_size(rest) >= length do
      true ->
        <<raw::binary-size(length), rest::binary>> = rest
        {raw, rest}

      false ->
        {nil, message}
    end
  end

  @impl true
  def deserialize(raw, socket) do
    {packet_id, rest} = VarInt.decode(raw, [])
    packet = PacketSchemas.decode(packet_id, rest, socket)

    if is_nil(packet) do
      raise "unable to decode packet with id #{inspect(packet_id)} - #{inspect(raw)}"
    end

    packet
  end
end
