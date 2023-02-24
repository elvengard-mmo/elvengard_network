defmodule ElvenGard.Protocol.Binary.IntegerType do
  @moduledoc ~S"""
  Define a custom integer type (uint32_t) for game protocols

  TODO: Manage signed/unsigned number & little/big/native endianness
  """

  use ElvenGard.FieldType

  # @impl ElvenGard.FieldType
  # @spec encode(integer, list) :: bitstring
  # def encode(int, _opts) do
  #   <<int::little-size(32)>>
  # end

  @impl ElvenGard.FieldType
  @spec decode(bitstring, list) :: {integer, bitstring}
  def decode(bin, _opts) do
    <<
      int::little-size(32),
      rest::binary
    >> = bin

    {int, rest}
  end
end
