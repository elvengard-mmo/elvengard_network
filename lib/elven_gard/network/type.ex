defmodule ElvenGard.Network.Type do
  @moduledoc ~S"""
  Define a behaviour for custom types (packet parsing)
  """

  @doc "Decode a field type"
  @callback decode(raw :: bitstring, opts :: keyword) :: {any, remaining :: bitstring}

  @doc "Encode a field type"
  @callback encode(value :: any, opts :: keyword) :: iodata

  defmacro __using__(_env) do
    quote location: :keep do
      @behaviour unquote(__MODULE__)

      @doc "Same as `decode/2` with empty options"
      @spec decode(raw :: bitstring) :: {any, remaining :: bitstring}
      def decode(raw), do: decode(raw)

      @doc "Same as `encode/2` with empty options"
      @spec encode(value :: any) :: iodata
      def encode(raw), do: encode(raw)
    end
  end
end
