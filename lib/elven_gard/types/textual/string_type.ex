defmodule ElvenGard.Types.Textual.StringType do
  @moduledoc """
  Define a custom string type for game protocols
  """

  use ElvenGard.Helpers.Type

  @impl ElvenGard.Helpers.Type
  @spec encode(String.t(), list) :: bitstring
  def encode(str, _opts) do
    str
  end

  @impl ElvenGard.Helpers.Type
  @spec decode(String.t(), list) :: String.t()
  def decode(str, _opts) do
    str
  end
end
