defmodule ElvenGard.Network.Endpoint.Connection do
  @moduledoc false

  alias ElvenGard.Network.PacketProcessor
  alias ElvenGard.Network.Socket
  alias ElvenGard.Network.SocketHandler

  @type result :: {:cont, Socket.t()} | {:halt, SocketHandler.stop_reason(), Socket.t()}
  @type init_result :: result() | {:cont, Socket.t(), timeout()}

  ## Public API

  @spec init(Socket.t(), module()) :: init_result()
  def init(%Socket{} = socket, socket_handler) do
    case socket_handler.handle_init(socket) do
      {:ok, %Socket{} = new_socket} ->
        {:cont, new_socket}

      {:ok, %Socket{} = new_socket, timeout}
      when timeout == :infinity or (is_integer(timeout) and timeout >= 0) ->
        {:cont, new_socket, timeout}

      {:stop, reason, %Socket{} = new_socket} ->
        {:halt, reason, new_socket}

      _ ->
        raise "handle_init/1 must return `{:ok, socket}`, `{:ok, socket, timeout}` " <>
                "or `{:stop, reason, socket}`"
    end
  end

  @spec info(any(), Socket.t(), module()) :: result()
  def info(message, %Socket{} = socket, socket_handler) do
    case socket_handler.handle_info(message, socket) do
      {:ok, %Socket{} = new_socket} ->
        {:cont, new_socket}

      {:stop, reason, %Socket{} = new_socket} ->
        {:halt, reason, new_socket}

      _ ->
        raise "handle_info/2 must return `{:ok, socket}` or `{:stop, reason, socket}`"
    end
  end

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

  @spec halt(SocketHandler.stop_reason(), Socket.t(), module()) :: Socket.t()
  def halt(reason, %Socket{} = socket, socket_handler) do
    case socket_handler.handle_halt(reason, socket) do
      {:ok, %Socket{} = new_socket} ->
        new_socket

      _ ->
        raise "handle_halt/2 must return `{:ok, socket}`"
    end
  end

  ## Private function

  defp process_packets(message, %Socket{} = socket, packet_handler) do
    %Socket{encoder: codec} = socket
    PacketProcessor.process(message, socket, codec, packet_handler)
  end
end
