defmodule ElvenGard.Network.Type do
  @moduledoc ~S"""
  Define a behaviour for custom types (packet parsing).

  This module defines a behaviour that allows users to create custom packet parsing
  types. By implementing the callbacks defined in this module, users can define
  how a specific field type is decoded and encoded when parsing and generating
  packets.

  To implement a custom type, you need to define the `decode/2` and `encode/2`
  callbacks. The `decode/2` function takes a raw binary as input and decodes it
  into a structured value of the custom type. The `encode/2` function takes a value
  of the custom type and encodes it into a binary representation.

  Note that the `ElvenGard.Network.PacketSerializer` module uses the callbacks in this
  behaviour when decoding packet fields. For each field defined in a packet serializer,
  the corresponding type's `decode/2` function will be called to parse the binary
  data.

  ## Example

  Here's an example of defining a custom type for decoding and encoding a 16-bit
  little-endian integer:

      defmodule MyLittleEndianIntType do
        use ElvenGard.Network.Type

        @impl ElvenGard.Network.Type
        def decode(raw, _opts) do
          <<value::little-unsigned-integer-size(16), rest::binary>> = raw
          {value, rest}
        end

        @impl ElvenGard.Network.Type
        def encode(value, _opts) do
          <<value::little-unsigned-integer-size(16)>>
        end
      end

  In the above example, we defined a custom type module `MyLittleEndianIntType` that
  decodes and encodes a 16-bit little-endian integer.

  Then, when defining a packet serializer using `ElvenGard.Network.PacketSerializer`, you
  can use this custom type to define a field in the packet structure.

  """

  ## Callbacks

  @doc """
  Decode a raw bitstring into a structured value of the custom type.

  Arguments:

  - `raw`: The raw bitstring to be decoded.
  - `opts`: A keyword list of options that can be used during decoding (optional).

  The function should return a tuple with the decoded value and the remaining
  unparsed bitstring.
  """
  @callback decode(raw :: bitstring, opts :: keyword) :: {any, remaining :: bitstring}

  @doc """
  Encode a value of the custom type into binary data.

  Arguments:

  - `value`: The value of the custom type to be encoded.
  - `opts`: A keyword list of options that can be used during encoding (optional).

  The function should return the encoded binary data as an iodata or a bitstring.
  """
  @callback encode(value :: any, opts :: keyword) :: iodata | bitstring

  defmacro __using__(_env) do
    quote location: :keep do
      @behaviour unquote(__MODULE__)

      @doc "Same as `decode/2` with empty options"
      @spec decode(raw :: bitstring) :: {any, remaining :: bitstring}
      def decode(raw), do: decode(raw, [])

      @doc "Same as `encode/2` with empty options"
      @spec encode(value :: any) :: iodata
      def encode(raw), do: encode(raw, [])
    end
  end
end
