defmodule ElvenGard.Network.ProtocolTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint.Protocol.Ranch
  alias ElvenGard.Network.Socket
  alias ElvenGard.Network.Socket.Adapters.Ranch, as: RanchAdapter

  Application.put_env(:elvengard_network, __MODULE__.MyEndpoint,
    listener_name: :my_endpoint,
    protocol: __MODULE__.MyProtocol,
    transport_opts: [ip: {127, 0, 0, 1}, port: 0]
  )

  Application.put_env(:elvengard_network, __MODULE__.MyProtocol,
    network_codec: __MODULE__.HaltCodec,
    packet_handler: __MODULE__.HaltHandler
  )

  defmodule HaltPacket do
    defstruct [:data]
  end

  defmodule HaltCodec do
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
    def decode(data, _socket), do: %HaltPacket{data: data}

    @impl true
    def encode(data, _socket), do: data
  end

  defmodule HaltHandler do
    @behaviour ElvenGard.Network.PacketHandler

    alias ElvenGard.Network.Socket

    @impl true
    def handle_packet(%HaltPacket{data: "halt"}, socket) do
      {:halt, Socket.assign(socket, :halted, true)}
    end

    def handle_packet(%HaltPacket{data: "halt:reason"}, socket) do
      {:halt, :requested, Socket.assign(socket, :halted, true)}
    end

    def handle_packet(%HaltPacket{}, socket), do: {:cont, socket}
  end

  defmodule HaltTransport do
    def close(pid) do
      send(pid, :transport_closed)
      :ok
    end

    def setopts(pid, opts) do
      send(pid, {:transport_opts, opts})
      :ok
    end
  end

  defmodule HaltProtocol do
    use ElvenGard.Network.SocketHandler

    @impl true
    def handle_message(_message, %{assigns: %{observer: observer}} = socket) do
      send(observer, {:remaining_seen_by_callback, socket.remaining})
      {:ok, socket}
    end

    def handle_message(_message, %{assigns: %{ignore_with_socket: true}} = socket) do
      {:ignore, %{socket | assigns: Map.put(socket.assigns, :ignored, true)}}
    end

    def handle_message(_message, %{assigns: %{ignore: true}}), do: :ignore

    def handle_message(_message, %{assigns: %{stop: true}} = socket) do
      {:stop, :requested, socket}
    end

    def handle_message(_message, socket), do: {:ok, socket}

    @impl true
    def handle_halt(reason, socket), do: {:ok, reason, socket}
  end

  defmodule MyEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule MyProtocol do
    use ElvenGard.Network.SocketHandler

    @impl true
    def handle_init(%Socket{} = socket) do
      # the first packet (link's pid) is prefixed with the packet length (cf. serialized_self/0)
      :ok = Socket.setopts(socket, packet: 1)
      {:ok, Socket.assign(socket, :connection_state, :waiting_for_link)}
    end

    @impl true
    def handle_message(message, %{assigns: %{connection_state: :waiting_for_link}} = socket) do
      link = :erlang.binary_to_term(message)
      :ok = Socket.setopts(socket, packet: 0)

      new_socket =
        Socket.assign(socket, %{connection_state: :connected, link: link})

      send(link, :handle_init)
      {:ignore, new_socket}
    end

    def handle_message(message, %{assigns: %{connection_state: :connected}} = socket) do
      %{assigns: %{link: link}} = socket
      send(link, {:handle_message, message})
      :ignore
    end

    @impl true
    def handle_halt(reason, %{assigns: %{link: _}} = socket) do
      %{assigns: %{link: link}} = socket
      send(link, {:handle_halt, reason})
      {:ok, socket}
    end
  end

  setup_all do
    start_supervised!(MyEndpoint)
    :ok
  end

  describe "c:handle_init/1" do
    test "is called" do
      _socket = connect()
      assert_receive :handle_init
    end
  end

  describe "c:handle_message/1" do
    test "is called" do
      socket = connect()
      send_data(socket, "test c:handle_message/1")

      assert_receive {:handle_message, "test c:handle_message/1"}
    end
  end

  describe "c:handle_halt/1" do
    test "is called" do
      socket = connect()
      close(socket)

      assert_receive {:handle_halt, :tcp_closed}
    end
  end

  describe "message processing" do
    test "receives the socket returned by the packet handler" do
      state = halt_state()

      assert {:stop, :normal, %Ranch.State{socket: %Socket{assigns: %{halted: true}}}} =
               Ranch.handle_info({:tcp, self(), "halt\n"}, state)

      assert_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end

    test "clears buffered data when the reconstructed message is ignored" do
      state = halt_state(remaining: "frag", assigns: %{ignore: true})

      assert {:noreply, %Ranch.State{socket: %Socket{remaining: <<>>}}} =
               Ranch.handle_info({:tcp, self(), "ment"}, state)

      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "clears buffered data after the reconstructed packet is consumed" do
      state = halt_state(remaining: "frag", assigns: %{observer: self()})

      assert {:noreply, %Ranch.State{socket: %Socket{remaining: <<>>}}} =
               Ranch.handle_info({:tcp, self(), "ment\n"}, state)

      assert_received {:remaining_seen_by_callback, "frag"}
      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "buffers an incomplete packet" do
      state = halt_state()

      assert {:noreply, %Ranch.State{socket: %Socket{remaining: "partial"}}} =
               Ranch.handle_info({:tcp, self(), "partial"}, state)

      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "preserves socket changes and clears buffered data with {:ignore, socket}" do
      state = halt_state(remaining: "frag", assigns: %{ignore_with_socket: true})

      assert {:noreply, %Ranch.State{socket: %Socket{remaining: <<>>, assigns: %{ignored: true}}}} =
               Ranch.handle_info({:tcp, self(), "ment"}, state)

      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "does not rearm the transport after handle_message/2 stops" do
      state = halt_state(assigns: %{stop: true})

      assert {:stop, :requested, ^state} =
               Ranch.handle_info({:tcp, self(), "stop"}, state)

      refute_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "passes the packet handler halt reason without rearming the transport" do
      state = halt_state()

      assert {:stop, :requested, %Ranch.State{socket: %Socket{assigns: %{halted: true}}}} =
               Ranch.handle_info({:tcp, self(), "halt:reason\n"}, state)

      assert_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end
  end

  ## Private/internals helpers

  defp halt_state(attrs \\ []) do
    socket =
      Socket.new(
        RanchAdapter,
        [transport: HaltTransport, socket: self()],
        HaltCodec
      )

    %Ranch.State{
      socket: struct!(socket, attrs),
      socket_handler: HaltProtocol,
      packet_handler: HaltHandler
    }
  end

  defp connect() do
    host = {127, 0, 0, 1}
    port = MyEndpoint.get_port()

    {:ok, socket} = :gen_tcp.connect(host, port, [:binary, active: true, packet: 0])
    :ok = :gen_tcp.send(socket, serialized_self())

    socket
  end

  defp close(socket) do
    :gen_tcp.close(socket)
  end

  defp send_data(socket, data) do
    :ok = :gen_tcp.send(socket, data)
  end

  defp serialized_self() do
    bin = :erlang.term_to_binary(self())
    length = byte_size(bin)
    <<length::8, bin::binary>>
  end
end
