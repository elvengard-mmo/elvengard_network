defmodule ElvenGard.Protocol.Binary.PaddingType do
  @moduledoc ~S"""
  Define a custom padding type for game protocols
  """

  use ElvenGard.FieldType

  # @impl ElvenGard.FieldType
  # @spec encode(term, list) :: bitstring
  # def encode(val, opts) do
  #   bits = Keyword.get(opts, :bits)
  #   bytes = Keyword.get(opts, :bytes)
  #   size = if bits != nil, do: bits, else: bytes * 8

  #   <<val::size(size)>>
  # end

  @impl ElvenGard.FieldType
  @spec decode(bitstring, list) :: {bitstring, bitstring}
  def decode(bin, opts) do
    fill = opts[:fill]
    bits = opts[:bits]
    bytes = opts[:bytes]

    if bits == nil and bytes == nil and fill == nil do
      raise ArgumentError, "you must specify a size (bytes/bits/fill) for a padding"
    end

    case {fill, bits, bytes} do
      {true, _, _} ->
        {bin, <<>>}

      {_, bits, _} when is_integer(bits) ->
        <<val::binary-size(bits)-unit(1), rest::binary>> = bin
        {val, rest}

      {_, _, bytes} when is_integer(bytes) ->
        <<val::binary-size(bytes), rest::binary>> = bin
        {val, rest}
    end
  end
end
