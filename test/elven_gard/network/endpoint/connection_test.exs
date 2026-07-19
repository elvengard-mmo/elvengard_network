defmodule ElvenGard.Network.Endpoint.ConnectionTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint.Connection
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
    def handle_init(%Socket{} = socket) do
      %Socket{assigns: assigns} = socket

      case Map.get(assigns, :init_action, :continue) do
        :continue -> {:ok, Socket.assign(socket, :initialized, true)}
        :timeout -> {:ok, Socket.assign(socket, :initialized, true), 250}
        :stop -> {:stop, :init_stop, Socket.assign(socket, :init_stopped, true)}
        :invalid -> :invalid
        :invalid_timeout -> {:ok, socket, :hibernate}
      end
    end

    @impl true
    def handle_message(message, %Socket{} = socket) do
      %Socket{assigns: assigns, remaining: remaining} = socket
      send(assigns.observer, {:raw_message, message, remaining})

      case Map.get(assigns, :message_action, :process) do
        :process -> {:ok, socket}
        :ignore -> :ignore
        :ignore_with_socket -> {:ignore, Socket.assign(socket, :raw_ignored, true)}
        :stop -> {:stop, :socket_stop, Socket.assign(socket, :socket_stopped, true)}
        :invalid -> :invalid
      end
    end

    @impl true
    def handle_info(message, %Socket{} = socket) do
      %Socket{assigns: assigns} = socket
      send(assigns.observer, {:info, message})

      case Map.get(assigns, :info_action, :continue) do
        :continue -> {:ok, Socket.assign(socket, :info, message)}
        :stop -> {:stop, :info_stop, Socket.assign(socket, :info_stopped, true)}
        :invalid -> :invalid
      end
    end

    @impl true
    def handle_halt(reason, %Socket{} = socket) do
      %Socket{assigns: assigns} = socket
      send(assigns.observer, {:connection_halt, reason})

      case Map.get(assigns, :halt_action, :continue) do
        :continue -> {:ok, Socket.assign(socket, :halted, true)}
        :legacy -> {:ok, :legacy_stop, socket}
        :invalid -> :invalid
      end
    end
  end

  test "normalizes socket handler initialization" do
    assert {:cont, %Socket{assigns: %{initialized: true}}} =
             Connection.init(socket(), SocketHandler)

    assert {:cont, %Socket{assigns: %{initialized: true}}, 250} =
             Connection.init(socket(assigns: %{init_action: :timeout}), SocketHandler)

    assert {:halt, :init_stop, %Socket{assigns: %{init_stopped: true}}} =
             Connection.init(socket(assigns: %{init_action: :stop}), SocketHandler)
  end

  test "rejects invalid socket handler initialization results" do
    assert_raise RuntimeError, ~r/handle_init\/1 must return/, fn ->
      Connection.init(socket(assigns: %{init_action: :invalid}), SocketHandler)
    end

    assert_raise RuntimeError, ~r/handle_init\/1 must return/, fn ->
      Connection.init(socket(assigns: %{init_action: :invalid_timeout}), SocketHandler)
    end
  end

  test "reconstructs fragmented data before processing packets" do
    socket = socket(remaining: "hel")

    assert {:cont, %Socket{remaining: <<>>, assigns: %{packet: "hello"}}} =
             Connection.process("lo\n", socket, SocketHandler, PacketHandler)

    assert_received {:raw_message, "hello\n", "hel"}
  end

  test "buffers an incomplete packet without pre-concatenating an empty remainder" do
    socket = socket()

    assert {:cont, %Socket{remaining: "partial"}} =
             Connection.process("partial", socket, SocketHandler, PacketHandler)

    assert_received {:raw_message, "partial", <<>>}
  end

  test "clears buffered data when raw handling ignores the message" do
    socket = socket(remaining: "old", assigns: %{message_action: :ignore})

    assert {:cont, %Socket{remaining: <<>>}} =
             Connection.process("new", socket, SocketHandler, PacketHandler)

    assert_received {:raw_message, "oldnew", "old"}
  end

  test "preserves socket changes when raw handling ignores with a socket" do
    socket = socket(remaining: "old", assigns: %{message_action: :ignore_with_socket})

    assert {:cont, %Socket{remaining: <<>>, assigns: %{raw_ignored: true}}} =
             Connection.process("new", socket, SocketHandler, PacketHandler)
  end

  test "returns socket handler and packet handler halts through the same contract" do
    socket_stop = socket(assigns: %{message_action: :stop})

    assert {:halt, :socket_stop, %Socket{assigns: %{socket_stopped: true}}} =
             Connection.process("stop", socket_stop, SocketHandler, PacketHandler)

    packet_stop = socket()

    assert {:halt, :packet_stop, %Socket{assigns: %{packet_halted: true}}} =
             Connection.process("halt\n", packet_stop, SocketHandler, PacketHandler)
  end

  test "rejects an invalid raw message result" do
    socket = socket(assigns: %{message_action: :invalid})

    assert_raise RuntimeError, ~r/invalid return value for handle_message\/2/, fn ->
      Connection.process("invalid", socket, SocketHandler, PacketHandler)
    end
  end

  test "normalizes socket handler info messages" do
    assert {:cont, %Socket{assigns: %{info: :event}}} =
             Connection.info(:event, socket(), SocketHandler)

    assert_received {:info, :event}

    assert {:halt, :info_stop, %Socket{assigns: %{info_stopped: true}}} =
             Connection.info(
               :event,
               socket(assigns: %{info_action: :stop}),
               SocketHandler
             )
  end

  test "rejects an invalid socket handler info result" do
    assert_raise RuntimeError, ~r/handle_info\/2 must return/, fn ->
      Connection.info(
        :event,
        socket(assigns: %{info_action: :invalid}),
        SocketHandler
      )
    end
  end

  test "normalizes halt cleanup and rejects legacy results" do
    assert %Socket{assigns: %{halted: true}} =
             Connection.halt(:closed, socket(), SocketHandler)

    assert_received {:connection_halt, :closed}

    assert_raise RuntimeError, ~r/handle_halt\/2 must return/, fn ->
      Connection.halt(:closed, socket(assigns: %{halt_action: :legacy}), SocketHandler)
    end

    assert_raise RuntimeError, ~r/handle_halt\/2 must return/, fn ->
      Connection.halt(:closed, socket(assigns: %{halt_action: :invalid}), SocketHandler)
    end
  end

  ## Private function

  defp socket(attrs \\ []) do
    assigns =
      %{observer: self()}
      |> Map.merge(Keyword.get(attrs, :assigns, %{}))

    %Socket{
      id: "connection-test",
      remaining: Keyword.get(attrs, :remaining, <<>>),
      assigns: assigns,
      encoder: Codec
    }
  end
end
