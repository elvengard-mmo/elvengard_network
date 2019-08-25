defmodule ElvenGard.Protocol.Binary.PaddingType do
  @moduledoc """
  Define a custom padding type for game protocols
  """

  use ElvenGard.Type

  alias ElvenGard.TypeOptionError

  @impl ElvenGard.Type
  @spec encode(term, list) :: bitstring
  def encode(val, opts) do
    bits = Keyword.get(opts, :bits)
    bytes = Keyword.get(opts, :bytes)
    size = if bits != nil, do: bits, else: bytes * 8

    <<val::size(size)>>
  end

  @impl ElvenGard.Type
  @spec decode(bitstring, list) :: {bitstring, bitstring}
  def decode(bin, opts) do
    fill = Keyword.get(opts, :fill)
    bits = Keyword.get(opts, :bits)
    bytes = Keyword.get(opts, :bytes)

    if bits == nil and bytes == nil and fill == nil do
      raise TypeOptionError, message: "You must specify a size (bytes/bits/fill) for a padding"
    end

    case {fill, bits, bytes} do
      {true, _, _} ->
        {bin, <<>>}

      {_, bits, _} when is_integer(bits) ->
        <<val::size(bits), rest::binary>> = bin
        {val, rest}

      {_, _, bytes} when is_integer(bytes) ->
        <<val::binary-size(bytes), rest::binary>> = bin
        {val, rest}
    end
  end
end
