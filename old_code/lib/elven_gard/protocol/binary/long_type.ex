defmodule ElvenGard.Protocol.Binary.LongType do
  @moduledoc ~S"""
  Define a custom long type (uint64_t) for game protocols

  TODO: Manage signed/unsigned number & little/big/native endianness
  """

  use ElvenGard.FieldType

  # @impl ElvenGard.FieldType
  # @spec encode(integer, list) :: bitstring
  # def encode(long, _opts) do
  #   <<long::little-size(64)>>
  # end

  @impl ElvenGard.FieldType
  @spec decode(bitstring, list) :: {integer, bitstring}
  def decode(bin, _opts) do
    <<
      long::little-size(64),
      rest::binary
    >> = bin

    {long, rest}
  end
end
