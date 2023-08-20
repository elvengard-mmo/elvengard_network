defmodule LoginServer.SubPackets.WorldInfo do
  @moduledoc """
  Documentation for LoginServer.SubPackets.WorldInfo
  """

  use ElvenGard.Network.Type

  alias __MODULE__
  alias LoginServer.Types.{IntegerType, StringType}

  @enforce_keys [:host, :port]
  defstruct [:host, :port]

  @type t :: %WorldInfo{host: StringType.t(), port: IntegerType.t()}

  ## Behaviour impls

  @impl true
  def decode(_data, _opts), do: raise("unimplemented")

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(data, opts) when is_struct(data, WorldInfo) do
    separator = Keyword.fetch!(opts, :sep)

    [
      StringType.encode(data.host),
      StringType.encode(separator),
      IntegerType.encode(data.port)
    ]
  end
end
