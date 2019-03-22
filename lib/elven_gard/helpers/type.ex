defmodule ElvenGard.Helpers.Type do
  @moduledoc """
  Define a behaviour for custom type (packet parsing)
  """

  @type encoded_term :: bitstring
  @type decoded_term :: term

  @callback encode(decoded_term) :: encoded_term
  @callback decode(encoded_term) :: {decoded_term, encoded_term}

  @doc false
  defmacro __using__(_) do
    parent = __MODULE__

    quote do
      @behaviour unquote(parent)

      @doc """
      Decode a bitstring named `bin` into a map and insert the result on it
      """
      @spec put_decode(map, atom) :: map
      def put_decode(%{bin: bin} = args, name) do
        {content, rest} = decode(bin)
        Map.put(%{args | bin: rest}, name, content)
      end
    end
  end
end
