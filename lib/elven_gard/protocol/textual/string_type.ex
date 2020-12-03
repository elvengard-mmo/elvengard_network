defmodule ElvenGard.Protocol.Textual.StringType do
  @moduledoc """
  Define a custom string type for text based protocols
  """

  use ElvenGard.Type

  ## Public API

  @impl true
  @spec encode(String.t(), keyword()) :: String.t()
  def encode(value, _opts) when is_binary(value), do: value

  def encode(value, _opts) do
    raise ArgumentError, "value to encode must be a string (got: #{inspect(value)})"
  end

  @impl true
  @spec decode(String.t(), keyword()) :: {String.t(), String.t()}
  def decode(raw, _opts) when not is_binary(raw) do
    raise ArgumentError, "value to decode must be a string (got: #{inspect(raw)})"
  end

  def decode(raw, separator: nil), do: norm_decode(raw)

  def decode(raw, separator: sep) when is_binary(sep) do
    raw
    |> String.split(sep, parts: 2)
    |> norm_decode()
  end

  def decode(_raw, separator: sep) do
    raise ArgumentError, "separator must be a string (got: #{inspect(sep)})"
  end

  def decode(raw, _), do: norm_decode(raw)

  ## Private functions

  @doc false
  @spec norm_decode(String.t() | [String.t(), ...]) :: {String.t(), String.t()}
  defp norm_decode([x, y]), do: {x, y}
  defp norm_decode([x]), do: {x, ""}
  defp norm_decode(x), do: {x, ""}
end
