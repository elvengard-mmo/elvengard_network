defmodule LoginServer.PacketEncoder do
  @moduledoc """
  Encode and decode a Login packet
  """

  use ElvenGard.Helpers.PacketEncoder

  require Logger

  alias LoginServer.Crypto

  @impl true
  @spec encode(String.t()) :: binary
  def encode(data) do
    Crypto.encrypt(data)
  end

  @impl true
  @spec decode(binary) :: [binary]
  def decode(data) do
    data
    |> Crypto.decrypt()
    |> String.replace("\n", "")
    |> String.split(:binary.compile_pattern([" ", "\v"]))
  end
end
