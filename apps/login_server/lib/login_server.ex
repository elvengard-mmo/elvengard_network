defmodule LoginServer do
  @moduledoc """
  Documentation for LoginServer.
  """

  use HeavensStrike.Game.LoginServer, port: 4002
  require Logger
  alias HeavensStrike.Game.Client
  alias LoginServer.Crypto

  def handle_init(args) do
    port = get_in(args, [:port])
    Logger.info("Login server started on port #{port}")
  end

  def handle_connection(%Client{id: id} = _client) do
    Logger.info("New connection: #{id}")
  end

  def handle_disconnection(%Client{id: id} = _client, reason) do
    Logger.info("#{id} is now disconnected (reason: #{inspect(reason)})")
  end

  # TODO: Remove this function
  def handle_message(%Client{id: id} = client, message) do
    Logger.info("New message from #{id}")
    pattern = :binary.compile_pattern([" ", "\v"])
    packet = message |> Crypto.decrypt() |> String.trim() |> String.split(pattern)
    LoginServer.PacketHandler.handle_packet(client, packet)
  end

  def handle_client_accepted(%Client{id: id} = client, args) do
    e_packet = Crypto.encrypt(args)

    Logger.info("Client accepted: #{id}")
    Client.send(client, e_packet)
  end

  def handle_client_refused(%Client{id: id} = client, reason) do
    e_reason = Crypto.encrypt("fail #{reason}")

    Logger.info("Client refused: #{id} - #{reason}")
    Client.send(client, e_reason)
  end
end
