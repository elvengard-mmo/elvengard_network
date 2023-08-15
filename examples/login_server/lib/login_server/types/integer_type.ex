defmodule LoginServer.Types.IntegerType do
  @moduledoc """
  Documentation for LoginServer.Types.IntegerType
  """

  use ElvenGard.Network.Type

  @type t :: integer()

  ## Behaviour impls

  @impl true
  def decode(_data, _opts), do: raise("unimplemented")

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(data, _opts) when is_integer(data) do
    Integer.to_string(data)
  end
end
