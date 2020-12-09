defmodule ElvenGard.Protocol.Binary.StringType do
  @moduledoc ~S"""
  Define a custom string type (utf8) for game protocols
  """

  use ElvenGard.FieldType

  # @impl ElvenGard.FieldType
  # @spec encode(String.t(), list) :: bitstring
  # def encode(str, _opts) do
  #   <<str::binary>>
  # end

  @impl ElvenGard.FieldType
  @spec decode(bitstring, list) :: {String.t(), bitstring}
  def decode(bin, opts) do
    bits = opts[:bits]
    bytes = opts[:bytes]

    if bits == nil and bytes == nil do
      raise ArgumentError, "you must specify a size in bytes/bits for a string"
    end

    size = if bits != nil, do: Kernel.trunc(bits / 8), else: bytes

    <<
      str::binary-size(size),
      rest::binary
    >> = bin

    {str, rest}
  end
end
