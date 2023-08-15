defmodule LoginServer.Types.DateTimeType do
  @moduledoc """
  Documentation for LoginServer.Types.DateTimeType
  """

  use ElvenGard.Network.Type

  @type t :: DateTime.t()

  ## Behaviour impls

  @impl true
  def decode(_data, _opts), do: raise("unimplemented")

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(data, _opts) when is_struct(data, DateTime) do
    DateTime.to_string(data)
  end
end
