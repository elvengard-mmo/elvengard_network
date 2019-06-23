defmodule ElvenGard.Protocol.Textual.StringType do
  @moduledoc """
  Define a custom string type for game protocols
  """

  use ElvenGard.Type

  @impl ElvenGard.Type
  @spec encode(String.t(), list) :: bitstring
  def encode(str, _opts) do
    str
  end

  @impl ElvenGard.Type
  @spec decode(String.t(), list) :: String.t()
  def decode(str, _opts) do
    str
  end
end
