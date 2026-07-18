defmodule ElvenGard.Network.Endpoint.MessageProcessor do
  @moduledoc false

  alias ElvenGard.Network.PacketProcessor
  alias ElvenGard.Network.Socket
  alias ElvenGard.Network.SocketHandler

  @type result ::
          {:cont, Socket.t()}
          | {:halt, SocketHandler.stop_reason(), Socket.t()}

  ## Public API

  @spec process(binary(), Socket.t(), module(), module()) :: result()
  def process(data, %Socket{} = socket, socket_handler, packet_handler) do
    %Socket{remaining: remaining} = socket

    message =
      case remaining do
        <<>> -> data
        _ -> :erlang.iolist_to_binary([remaining | data])
      end

    case socket_handler.handle_message(message, socket) do
      :ignore ->
        {:cont, %Socket{socket | remaining: <<>>}}

      {:ignore, %Socket{} = new_socket} ->
        {:cont, %Socket{new_socket | remaining: <<>>}}

      {:ok, %Socket{} = new_socket} ->
        process_packets(message, new_socket, packet_handler)

      {:stop, reason, %Socket{} = new_socket} ->
        {:halt, reason, new_socket}

      value ->
        raise "invalid return value for handle_message/2 (got: #{inspect(value)})"
    end
  end

  ## Private function

  defp process_packets(message, %Socket{} = socket, packet_handler) do
    %Socket{encoder: codec} = socket
    PacketProcessor.process(message, socket, codec, packet_handler)
  end
end
