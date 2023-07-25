defmodule MinecraftEx.Endpoint.PacketHandlers do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.PacketHandlers
  """

  import ElvenGard.Network.Socket, only: [assign: 3]

  alias ElvenGard.Network.Socket
  alias MinecraftEx.Resources

  alias MinecraftEx.Endpoint.PacketSchemas.{
    Handshake,
    PingRequest,
    StatusRequest
  }

  alias MinecraftEx.Types.{Long, MCString}

  def handle_packet(%Handshake{} = packet, socket) do
    {:cont, assign(socket, :state, packet.next_state)}
  end

  def handle_packet(%StatusRequest{}, socket) do
    json =
      Poison.encode!(%{
        version: %{
          name: "1.20.1-ex",
          protocol: 763
        },
        players: %{
          max: 100,
          online: 1,
          sample: [
            %{
              name: "DarkyZ",
              id: "4566e69f-c907-48ee-8d71-d7ba5aa00d20"
            }
          ]
        },
        description: [
          %{text: "Hello from "},
          %{
            text: "Elixir",
            color: "dark_purple"
          },
          %{text: "!\n"},
          %{
            text: "ElixirElixirElixirElixirElixirElixirElixir",
            obfuscated: true
          }
        ],
        favicon: Resources.favicon(),
        enforcesSecureChat: true,
        previewsChat: true
      })

    render = MCString.encode(json)
    packet_id = 0

    packet = [<<packet_id::8>>, render]
    Socket.send(socket, packet)

    {:cont, socket}
  end

  def handle_packet(%PingRequest{payload: payload}, socket) do
    render = Long.encode(payload)
    packet_id = 1

    packet = [<<packet_id::8>>, render]
    Socket.send(socket, packet)

    {:halt, socket}
  end

  def handle_packet(packet, socket) do
    IO.warn("no handler for #{inspect(packet)}")
    {:halt, socket}
  end
end
