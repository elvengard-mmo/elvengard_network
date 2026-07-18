defmodule ElvenGard.Network.PacketProcessor do
  @moduledoc """
  Decodes and handles every complete packet available in a binary.

  This module is independent from the underlying socket adapter. It preserves
  incomplete trailing data in the socket and exposes packet-handler halts as a
  transport-agnostic result.
  """

  alias ElvenGard.Network.PacketHandler
  alias ElvenGard.Network.Socket

  @type result ::
          {:cont, Socket.t()}
          | {:halt, PacketHandler.halt_reason(), Socket.t()}

  @spec process(binary(), Socket.t(), module(), module()) :: result()
  def process(<<>>, %Socket{} = socket, _codec, _packet_handler) do
    {:cont, %Socket{socket | remaining: <<>>}}
  end

  def process(data, %Socket{} = socket, codec, packet_handler) do
    case codec.next(data, socket) do
      {nil, rest} when is_binary(rest) ->
        {:cont, %Socket{socket | remaining: rest}}

      {raw, rest} when is_binary(raw) and is_binary(rest) ->
        packet = codec.decode(raw, socket)

        case packet_handler.handle_packet(packet, socket) do
          {:cont, %Socket{} = new_socket} ->
            process(rest, new_socket, codec, packet_handler)

          {:halt, %Socket{} = new_socket} ->
            {:halt, :normal, new_socket}

          {:halt, reason, %Socket{} = new_socket} ->
            {:halt, reason, new_socket}
        end
    end
  end
end
