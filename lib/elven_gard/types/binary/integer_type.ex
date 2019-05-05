defmodule ElvenGard.Types.Binary.IntegerType do
  @moduledoc """
  Define a custom integer type (uint32_t) for game protocols
  """

  use ElvenGard.Helpers.Type

  @impl ElvenGard.Helpers.Type
  @spec encode(integer, list) :: bitstring
  def encode(int, _opts) do
    <<int::little-size(32)>>
  end

  @impl ElvenGard.Helpers.Type
  @spec decode(bitstring, list) :: {integer, bitstring}
  def decode(bin, _opts) do
    <<
      int::little-size(32),
      rest::binary
    >> = bin

    {int, rest}
  end
end
