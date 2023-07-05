defmodule MinecraftEx.Endpoint.Protocol do
  @moduledoc """
  Documentation for MinecraftEx.Endpoint.Protocol
  """

  use ElvenGard.Network.Endpoint.Protocol

  require Logger

  alias ElvenGard.Network.Socket

  alias MinecraftEx.Resources

  alias MinecraftEx.Types.{
    Boolean,
    Long,
    String,
    Short,
    UUID,
    VarInt
  }

  ## Callbacks

  @impl true
  def handle_init(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    Logger.metadata(socket_id: socket.id)

    %Socket{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :raw, reuseaddr: true)

    {:ok, socket}
  end

  @impl true
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message (len: #{byte_size(message)})")

    message
    |> decode()
    |> Enum.each(&handle_packet(&1, socket))

    :ignore
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end

  ## Helpers

  @decoders %{
    0x00 => [:do_decode_00_0, :do_decode_00_1, :do_decode_00_2],
    0x01 => :do_decode_01_0
  }

  defp decode(message, result \\ [])
  defp decode(<<>>, result), do: Enum.reverse(result)

  defp decode(message, result) do
    {length, rest} = VarInt.decode(message, [])

    if bit_size(rest) < length do
      Enum.reverse(result)
    else
      <<packet::binary-size(length), rest_packet::bitstring>> = rest
      {packet_id, rest} = VarInt.decode(packet, [])

      packet_struct =
        case @decoders[packet_id] do
          nil ->
            raise "no decoder found for packet with id #{inspect(packet_id)}"

          decoder when is_atom(decoder) ->
            apply(__MODULE__, decoder, [packet_id, rest])

          decoders when is_list(decoders) ->
            Enum.find_value(decoders, fn decoder ->
              try do
                apply(__MODULE__, decoder, [packet_id, rest])
              rescue
                _ -> nil
              end
            end)
        end

      if is_nil(packet_struct) do
        raise("unable to decode packet with id #{inspect(packet_id)} - #{inspect(packet)}")
      end

      packet_struct = elem(packet_struct, 0)
      IO.inspect(packet_struct, label: "packet_struct")

      decode(rest_packet, [packet_struct | result])
    end
  end

  # 0x00 Handshake
  def do_decode_00_0(packet_id, data) do
    # 1.20.1 	763
    {protocol_version, rest} = VarInt.decode(data, [])
    {server_address, rest} = String.decode(rest, [])
    {server_port, rest} = Short.decode(rest, sign: :signed)
    {next_state, rest} = VarInt.decode(rest, [])

    if rest != "", do: raise("invalid data decoding: remains #{inspect(rest)}")

    data = %{
      packet_id: packet_id,
      protocol_version: protocol_version,
      server_address: server_address,
      server_port: server_port,
      next_state: next_state
    }

    {data, rest}
  end

  # 0x00 Handshake - Status Request
  def do_decode_00_1(packet_id, <<>>) do
    {%{packet_id: packet_id}, <<>>}
  end

  # 0x00 Login Start
  def do_decode_00_2(packet_id, data) do
    {name, rest} = String.decode(data, [])

    if Elixir.String.length(name) > 16, do: raise("invalid player name")

    {player_uuid?, rest} = Boolean.decode(rest, [])

    {player_uuid, rest} =
      case player_uuid? do
        false -> {nil, rest}
        true -> UUID.decode(rest, [])
      end

    {%{packet_id: packet_id, player_uuid: player_uuid}, rest}
  end

  # 0x01 Ping Request
  def do_decode_01_0(packet_id, data) do
    {ack, rest} = Long.decode(data, [])

    if rest != "", do: raise("invalid data decoding: remains #{inspect(rest)}")

    {%{packet_id: packet_id, ack: ack}, rest}
  end

  ## Handlers

  defp handle_packet(%{packet_id: 0x00} = packet, socket) when map_size(packet) == 1 do
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

    render = String.encode(json, [])
    packet_length = VarInt.encode(byte_size(render) + 1, [])
    packet_id = 0

    packet = <<packet_length::binary, packet_id::8, render::binary>>
    Socket.send(socket, packet)
  end

  defp handle_packet(%{packet_id: 0x01, ack: ack}, socket) do
    render = Long.encode(ack, [])
    packet_length = VarInt.encode(byte_size(render) + 1, [])
    packet_id = 1

    packet = <<packet_length::binary, packet_id::8, render::binary>>
    Socket.send(socket, packet)
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

  defp handle_packet(packet, _socket) do
    IO.warn("no handler for #{inspect(packet)}")
  end
end
