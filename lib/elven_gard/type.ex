defmodule ElvenGard.Type do
  @moduledoc """
  Define a behaviour for custom type (packet parsing)
  """

  @type encoded_term :: bitstring
  @type decoded_term :: term

  @callback encode(decoded_term, opts :: list) :: term
  @callback decode(encoded_term, opts :: list) :: term

  @doc false
  defmacro __using__(_) do
    parent = __MODULE__

    quote do
      @behaviour unquote(parent)

      @doc """
      Encode a bitstring without params
      """
      def encode(val), do: encode(val, [])
    end
  end
end
