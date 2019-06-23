defmodule ElvenGard.Protocol.Binary.LongType do
  @moduledoc """
  Define a custom long type (uint64_t) for game protocols
  """

  use ElvenGard.Type

  @impl ElvenGard.Type
  @spec encode(integer, list) :: bitstring
  def encode(long, _opts) do
    <<long::little-size(64)>>
  end

  @impl ElvenGard.Type
  @spec decode(bitstring, list) :: {integer, bitstring}
  def decode(bin, _opts) do
    <<
      long::little-size(64),
      rest::binary
    >> = bin

    {long, rest}
  end
end
