defmodule MinecraftEx.Types.Long do
  @moduledoc """
  An integer between -9223372036854775808 and 9223372036854775807

  ===

  Signed 64-bit integer, two's complement 
  """

  use ElvenGard.Network.Type

  @type t :: -9_223_372_036_854_775_808..9_223_372_036_854_775_807

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, _opts) when is_binary(data) do
    <<value::signed-64, rest::bitstring>> = data
    {value, rest}
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(data, _opts) when is_integer(data) do
    <<data::signed-64>>
  end
end
