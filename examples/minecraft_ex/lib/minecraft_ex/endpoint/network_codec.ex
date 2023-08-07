defmodule MinecraftEx.Endpoint.NetworkCodec do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.NetworkCodec
  """

  @behaviour ElvenGard.Network.NetworkCodec

  alias MinecraftEx.Types.VarInt
  alias MinecraftEx.ClientPackets

  @impl true
  def next(<<>>), do: {nil, <<>>}

  def next(message) do
    {length, rest} = VarInt.decode(message)

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
    {packet_id, rest} = VarInt.decode(raw)
    packet = ClientPackets.decode(packet_id, rest, socket)

    if is_nil(packet) do
      raise "unable to decode packet with id #{inspect(packet_id)} - #{inspect(raw)}"
    end

    packet
  end

  @impl true
  def serialize(raw, _socket) do
    packet_length =
      raw |> List.wrap() |> Enum.map(&byte_size/1) |> Enum.sum() |> VarInt.encode([])

    [<<packet_length::binary>> | raw]
  end
end
