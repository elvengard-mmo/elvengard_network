defmodule ElvenGard.Network.Endpoint.MessageProcessorTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint.MessageProcessor
  alias ElvenGard.Network.Socket

  defmodule Packet do
    defstruct [:data]
  end

  defmodule Codec do
    @behaviour ElvenGard.Network.NetworkCodec

    ## NetworkCodec callbacks

    @impl true
    def next(data, _socket) do
      case :binary.match(data, "\n") do
        {packet_size, 1} ->
          <<packet::binary-size(^packet_size), "\n", rest::binary>> = data
          {packet, rest}

        :nomatch ->
          {nil, data}
      end
    end

    @impl true
    def decode(data, _socket), do: %Packet{data: data}

    @impl true
    def encode(%Packet{data: data}, _socket), do: [data, "\n"]
  end

  defmodule PacketHandler do
    @behaviour ElvenGard.Network.PacketHandler

    ## PacketHandler callbacks

    @impl true
    def handle_packet(%Packet{} = packet, socket) do
      %Packet{data: data} = packet

      case data do
        "halt" -> {:halt, :packet_stop, Socket.assign(socket, :packet_halted, true)}
        _ -> {:cont, Socket.assign(socket, :packet, data)}
      end
    end
  end

  defmodule SocketHandler do
    use ElvenGard.Network.SocketHandler

    ## SocketHandler callbacks

    @impl true
    def handle_message(message, %Socket{} = socket) do
      %Socket{assigns: assigns, remaining: remaining} = socket
      send(assigns.observer, {:raw_message, message, remaining})

      case Map.get(assigns, :action, :process) do
        :process -> {:ok, socket}
        :ignore -> :ignore
        :ignore_with_socket -> {:ignore, Socket.assign(socket, :raw_ignored, true)}
        :stop -> {:stop, :socket_stop, Socket.assign(socket, :socket_stopped, true)}
        :invalid -> :invalid
      end
    end
  end

  test "reconstructs fragmented data before processing packets" do
    socket = socket(remaining: "hel")

    assert {:cont, %Socket{remaining: <<>>, assigns: %{packet: "hello"}}} =
             MessageProcessor.process("lo\n", socket, SocketHandler, PacketHandler)

    assert_received {:raw_message, "hello\n", "hel"}
  end

  test "buffers an incomplete packet without pre-concatenating an empty remainder" do
    socket = socket()

    assert {:cont, %Socket{remaining: "partial"}} =
             MessageProcessor.process("partial", socket, SocketHandler, PacketHandler)

    assert_received {:raw_message, "partial", <<>>}
  end

  test "clears buffered data when raw handling ignores the message" do
    socket = socket(remaining: "old", assigns: %{action: :ignore})

    assert {:cont, %Socket{remaining: <<>>}} =
             MessageProcessor.process("new", socket, SocketHandler, PacketHandler)

    assert_received {:raw_message, "oldnew", "old"}
  end

  test "preserves socket changes when raw handling ignores with a socket" do
    socket = socket(remaining: "old", assigns: %{action: :ignore_with_socket})

    assert {:cont, %Socket{remaining: <<>>, assigns: %{raw_ignored: true}}} =
             MessageProcessor.process("new", socket, SocketHandler, PacketHandler)
  end

  test "returns socket handler and packet handler halts through the same contract" do
    socket_stop = socket(assigns: %{action: :stop})

    assert {:halt, :socket_stop, %Socket{assigns: %{socket_stopped: true}}} =
             MessageProcessor.process("stop", socket_stop, SocketHandler, PacketHandler)

    packet_stop = socket()

    assert {:halt, :packet_stop, %Socket{assigns: %{packet_halted: true}}} =
             MessageProcessor.process("halt\n", packet_stop, SocketHandler, PacketHandler)
  end

  test "rejects an invalid raw message result" do
    socket = socket(assigns: %{action: :invalid})

    assert_raise RuntimeError, ~r/invalid return value for handle_message\/2/, fn ->
      MessageProcessor.process("invalid", socket, SocketHandler, PacketHandler)
    end
  end

  ## Private function

  defp socket(attrs \\ []) do
    assigns =
      %{observer: self()}
      |> Map.merge(Keyword.get(attrs, :assigns, %{}))

    %Socket{
      id: "message-processor-test",
      remaining: Keyword.get(attrs, :remaining, <<>>),
      assigns: assigns,
      encoder: Codec
    }
  end
end
