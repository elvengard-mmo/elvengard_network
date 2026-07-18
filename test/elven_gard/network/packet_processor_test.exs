defmodule ElvenGard.Network.PacketProcessorTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.PacketProcessor
  alias ElvenGard.Network.Socket

  defmodule Packet do
    defstruct [:data]
  end

  defmodule Codec do
    @behaviour ElvenGard.Network.NetworkCodec

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
    def encode(data, _socket), do: data
  end

  defmodule Handler do
    @behaviour ElvenGard.Network.PacketHandler

    alias ElvenGard.Network.Socket

    @impl true
    def handle_packet(%Packet{data: "halt"}, %Socket{} = socket) do
      {:halt, Socket.assign(socket, :halted, true)}
    end

    def handle_packet(%Packet{data: "halt:reason"}, %Socket{} = socket) do
      {:halt, :requested, Socket.assign(socket, :halted, true)}
    end

    def handle_packet(%Packet{} = packet, %Socket{} = socket) do
      %Packet{data: data} = packet
      %Socket{assigns: assigns} = socket
      observer = Map.fetch!(assigns, :observer)
      handled_count = Map.get(assigns, :handled_count, 0)

      send(observer, {:handled_packet, data, handled_count})
      {:cont, Socket.assign(socket, :handled_count, handled_count + 1)}
    end
  end

  test "clears previously buffered data when all data is consumed" do
    socket = %Socket{remaining: "stale"}

    assert {:cont, %Socket{remaining: <<>>}} =
             PacketProcessor.process(<<>>, socket, Codec, Handler)
  end

  test "buffers an incomplete packet" do
    socket = %Socket{}

    assert {:cont, %Socket{remaining: "partial"}} =
             PacketProcessor.process("partial", socket, Codec, Handler)
  end

  test "handles every complete packet and buffers the trailing fragment" do
    socket = %Socket{assigns: %{observer: self()}}

    assert {:cont, %Socket{remaining: "partial", assigns: %{handled_count: 2}}} =
             PacketProcessor.process("one\ntwo\npartial", socket, Codec, Handler)

    assert_received {:handled_packet, "one", 0}
    assert_received {:handled_packet, "two", 1}
  end

  test "returns a normal halt with the socket returned by the handler" do
    socket = %Socket{}

    assert {:halt, :normal, %Socket{assigns: %{halted: true}}} =
             PacketProcessor.process("halt\n", socket, Codec, Handler)
  end

  test "preserves the halt reason and socket returned by the handler" do
    socket = %Socket{}

    assert {:halt, :requested, %Socket{assigns: %{halted: true}}} =
             PacketProcessor.process("halt:reason\n", socket, Codec, Handler)
  end
end
