defmodule MinecraftEx.Endpoint.Protocol do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.Protocol
  """

  use ElvenGard.Network.Endpoint.Protocol

  require Logger

  import ElvenGard.Network.Socket, only: [assign: 3]

  alias ElvenGard.Network.Socket
  alias MinecraftEx.Resources

  alias MinecraftEx.Types.{
    Long,
    MCString,
    VarInt
  }

  ## Callbacks

  @impl true
  def handle_init(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    Logger.metadata(socket_id: socket.id)

    %Socket{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :raw, reuseaddr: true)

    {:ok, assign(socket, :state, :init)}
  end

  @impl true
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message (len: #{byte_size(message)})")

    {updated_socket, _rest} = packet_loop(message, socket)

    {:ignore, updated_socket}
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end

  ## Helpers

  defp packet_loop(data, socket) do
    case apply(codec(), :next, [data]) do
      {nil, rest} ->
        {socket, rest}

      {raw, rest} ->
        raw
        |> then(&apply(codec(), :deserialize, [&1, socket]))
        |> handle_packet(socket)
        |> then(&packet_loop(rest, &1))
    end
  end

  ## Handlers

  defp codec(), do: Application.fetch_env!(:minecraft_ex, MinecraftEx.Endpoint)[:packet_codec]

  defp handle_packet(%{packet_name: Handshake} = packet, socket) do
    assign(socket, :state, packet.next_state)
  end

  defp handle_packet(%{packet_name: StatusRequest}, socket) do
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

    render = MCString.encode(json, [])
    packet_length = VarInt.encode(byte_size(render) + 1, [])
    packet_id = 0

    packet = <<packet_length::binary, packet_id::8, render::binary>>
    Socket.send(socket, packet)

    socket
  end

  defp handle_packet(%{packet_name: PingRequest, payload: payload}, socket) do
    render = Long.encode(payload, [])
    packet_length = VarInt.encode(byte_size(render) + 1, [])
    packet_id = 1

    packet = <<packet_length::binary, packet_id::8, render::binary>>
    Socket.send(socket, packet)

    socket
  end

  # Encryption Response
  # defp handle_packet(%{packet_id: 0x00, player_uuid: player_uuid}, socket) do
  #   render = 
  #     Long.encode(ack, [])

  #   # packet_length = VarInt.encode(byte_size(render) + 1, [])
  #   # packet_id = 1

  #   # packet = <<packet_length::binary, packet_id::8, render::binary>>
  #   # Socket.send(socket, packet)
  # end

  defp handle_packet(packet, socket) do
    IO.warn("no handler for #{inspect(packet)}")
    socket
  end
end
