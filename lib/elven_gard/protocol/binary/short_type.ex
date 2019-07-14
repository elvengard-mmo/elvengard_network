defmodule ElvenGard.Protocol.Binary.ShortType do
  @moduledoc """
  Define a custom short type (uint16_t) for game protocols

  TODO: Manage signed/unsigned number & little/big/native endianness
  """

  use ElvenGard.Type

  @impl ElvenGard.Type
  @spec encode(integer, list) :: bitstring
  def encode(short, _opts) do
    <<short::little-size(16)>>
  end

  @impl ElvenGard.Type
  @spec decode(bitstring, list) :: {integer, bitstring}
  def decode(bin, _opts) do
    <<
      short::little-size(16),
      rest::binary
    >> = bin

    {short, rest}
  end
end
