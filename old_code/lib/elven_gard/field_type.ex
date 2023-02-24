defmodule ElvenGard.FieldType do
  @moduledoc ~S"""
  Define a behaviour for custom type (packet parsing)
  """

  @typedoc "Represents a part of a raw packet received by a client"
  @type encoded_term() :: bitstring()
  @typedoc "Represents the term that this class provides"
  @type decoded_term() :: any()

  @doc """
  Transforms a packet received by a client into a term that can be used by `ElvenGard.Protocol`

  This functions returns a tuple with 2 elements. The first one is the decoded term and the
  second is the remaining bytes.

  NOTE: The result of this function will then be used by `c:ElvenGard.Protocol.decode/2`
  """
  @callback decode(raw :: encoded_term(), opts :: keyword()) :: {decoded_term(), encoded_term()}

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)

      @typep encoded_term() :: unquote(__MODULE__).encoded_term()
      @typep decoded_term() :: unquote(__MODULE__).decoded_term()

      @doc """
      Decode a bitstring without passing any options
      """
      @spec decode(raw :: encoded_term()) :: {decoded_term(), encoded_term()}
      def decode(val), do: decode(val, [])
    end
  end
end
