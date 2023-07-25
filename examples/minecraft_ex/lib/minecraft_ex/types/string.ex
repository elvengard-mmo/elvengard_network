defmodule MinecraftEx.Types.MCString do
  @moduledoc """
  A sequence of Unicode scalar values

  ===

  UTF-8 string prefixed with its size in bytes as a VarInt. 
  Maximum length of n characters, which varies by context; up to n × 4 
  bytes can be used to encode n characters and both of those limits are 
  checked. Maximum n value is 32767. The + 3 is due to the max size of 
  a valid length VarInt. 
  """

  alias MinecraftEx.Types.VarInt

  use ElvenGard.Network.Type

  @type t :: String.t()

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, _opts) when is_binary(data) do
    {length, rest} = VarInt.decode(data)

    if length > 32767, do: raise("max string size is 32767")

    <<string::binary-size(length), rest::bitstring>> = rest
    {string, rest}
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(data, _opts) when is_binary(data) do
    length = data |> byte_size() |> VarInt.encode([])
    <<length::binary, data::binary>>
  end
end
