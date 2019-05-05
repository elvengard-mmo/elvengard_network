defmodule ElvenGard.Types.Textual.FloatType do
  @moduledoc """
  Define a custom float type for game protocols
  """

  use ElvenGard.Helpers.Type

  @impl ElvenGard.Helpers.Type
  @spec encode(float, list) :: String.t()
  def encode(number, _opts) do
    Float.to_string(number)
  end

  @impl ElvenGard.Helpers.Type
  @spec decode(String.t(), list) :: float
  def decode(str, _opts) do
    String.to_float(str)
  end
end
