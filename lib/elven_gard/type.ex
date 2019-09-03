defmodule ElvenGard.Type do
  @moduledoc """
  Define a behaviour for custom type (packet parsing)
  """

  @typedoc "Represents a part of a raw packet received by the client"
  @type encoded_term :: bitstring
  @typedoc "Represents the term that this class provides"
  @type decoded_term :: term

  @doc """
  Transforms a term into a packet that can be sent to the client
  """
  @callback encode(decoded_term, opts :: list) :: term

  @doc """
  Transforms a packet received by a client into a term that can be used by `ElvenGard.Protocol`

  NOTE: The result of this function will then be used by `c:ElvenGard.Protocol.decode/1`
  """
  @callback decode(encoded_term, opts :: list) :: term

  @doc false
  defmacro __using__(_) do
    parent = __MODULE__

    quote do
      @behaviour unquote(parent)

      @doc """
      Encode a term without passing any options
      """
      def encode(val), do: encode(val, [])

      @doc """
      Decode a bitstring without passing any options
      """
      def decode(val), do: decode(val, [])
    end
  end
end
