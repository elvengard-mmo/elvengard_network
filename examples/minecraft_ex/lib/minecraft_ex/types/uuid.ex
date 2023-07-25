defmodule MinecraftEx.Types.UUID do
  @moduledoc """
  A UUID

  ===

  Encoded as an unsigned 128-bit integer (or two unsigned 64-bit integers: 
  the most significant 64 bits and then the least significant 64 bits) 
  """

  use ElvenGard.Network.Type

  @type t :: String.t()

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, _opts) when is_binary(data) do
    <<data::binary-16, rest::bitstring>> = data
    {ElvenGard.Network.UUID.uuid_to_string(data), rest}
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(_data, _opts), do: raise("unimplemented for now")
end
