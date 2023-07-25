defmodule MinecraftEx.Types.Boolean do
  @moduledoc """
  Either false or true

  ===

  True is encoded as 0x01, false as 0x00.
  """

  use ElvenGard.Network.Type

  @type t :: boolean

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, _opts) when is_binary(data) do
    <<value::8, rest::bitstring>> = data
    if value not in [0, 1], do: raise("invalid boolean value: #{inspect(value)}")
    {value == 0x01, rest}
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(data, _opts) when is_boolean(data) do
    case data do
      true -> <<1::8>>
      false -> <<0::8>>
    end
  end
end
