defmodule ElvenGard.Protocol.Textual.IntegerType do
  @moduledoc """
  Define a custom integer type for text based protocols
  """

  use ElvenGard.Type

  ## Public API

  @impl true
  @spec encode(integer(), keyword()) :: String.t()
  def encode(value, _opts) when is_integer(value), do: Integer.to_string(value)

  def encode(value, _opts) do
    raise ArgumentError, "the value to encode must be an integer (got: #{inspect(value)})"
  end

  @impl true
  @spec decode(String.t(), keyword()) :: {integer(), String.t()}
  def decode(raw, _opts) when not is_binary(raw) do
    raise ArgumentError, "value to decode must be a string (got: #{inspect(raw)})"
  end

  def decode(raw, []), do: norm_decode(raw)
  def decode(raw, separator: nil), do: norm_decode(raw)

  def decode(raw, separator: sep) when is_binary(sep) do
    raw
    |> String.split(sep, parts: 2)
    |> norm_decode()
  end

  def decode(_raw, separator: sep) do
    raise ArgumentError, "separator must be a string (got: #{inspect(sep)})"
  end

  ## Private functions

  @doc false
  @spec norm_decode(String.t() | [String.t(), ...]) :: {integer(), String.t()}
  defp norm_decode([x, y]), do: {String.to_integer(x), y}
  defp norm_decode([x]), do: {String.to_integer(x), ""}
  defp norm_decode(x), do: {String.to_integer(x), ""}
end
