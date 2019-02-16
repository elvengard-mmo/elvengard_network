defmodule LoginServer.PacketEncoder do
  @moduledoc """
  Parse a packet and call the right PacketHandler
  """

  use ElvenGard.Helpers.PacketEncoder,
    packet_handler: LoginServer.PacketHandler

  require Logger
  alias LoginServer.Crypto

  @impl true
  def encode(data) do
    data
    |> Crypto.encrypt()
  end

  @impl true
  def decode(data) do
    data
    |> Crypto.decrypt()
    |> String.replace("\n", "")
    |> String.split(:binary.compile_pattern([" ", "\v"]))
  end
end
