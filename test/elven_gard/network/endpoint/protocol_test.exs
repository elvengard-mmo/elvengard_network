defmodule ElvenGard.Network.ProtocolTest do
  use ExUnit.Case, async: true

  Application.put_env(:elvengard_network, __MODULE__.MyEndpoint,
    listener_name: :my_endpoint,
    protocol: __MODULE__.MyProtocol,
    transport_opts: [ip: {127, 0, 0, 1}, port: 0]
  )

  Application.put_env(:elvengard_network, __MODULE__.MyProtocol, [])

  Application.put_env(:elvengard_network, __MODULE__.HaltProtocol,
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

    def handle_packet(%HaltPacket{data: data}, %{assigns: %{packet_observer: observer}} = socket) do
      send(observer, {:handled_packet, data})
      {:cont, socket}
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
    use ElvenGard.Network.Endpoint.Protocol

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
    use ElvenGard.Network.Endpoint.Protocol

    import ElvenGard.Network.Socket, only: [assign: 3]

    alias ElvenGard.Network.Socket

    @timeout 100

    @impl true
    def handle_init(%Socket{transport: transport, transport_pid: transport_pid} = socket) do
      # the first packet (link's pid) is prefixed with the packet length (cf. serialized_self/0)
      Socket.setopts(socket, packet: 1)
      {:ok, bin} = transport.recv(transport_pid, 0, @timeout)
      Socket.setopts(socket, packet: 0)

      link = :erlang.binary_to_term(bin)

      send(link, :handle_init)
      {:ok, assign(socket, :link, link)}
    end

    @impl true
    def handle_message(message, socket) do
      send(socket.assigns[:link], {:handle_message, message})
      :ignore
    end

    @impl true
    def handle_halt(reason, socket) do
      send(socket.assigns[:link], {:handle_halt, reason})
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
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        encoder: HaltCodec
      }

      assert {:stop, :normal, %{assigns: %{halted: true}}} =
               HaltProtocol.handle_info({:tcp, self(), "halt\n"}, socket)

      assert_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end

    test "clears buffered data when the reconstructed message is ignored" do
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        remaining: "frag",
        assigns: %{ignore: true},
        encoder: HaltCodec
      }

      assert {:noreply, %{remaining: <<>>}} =
               HaltProtocol.handle_info({:tcp, self(), "ment"}, socket)

      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "clears buffered data after the reconstructed packet is consumed" do
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        remaining: "frag",
        assigns: %{observer: self()},
        encoder: HaltCodec
      }

      assert {:noreply, %{remaining: <<>>}} =
               HaltProtocol.handle_info({:tcp, self(), "ment\n"}, socket)

      assert_received {:remaining_seen_by_callback, "frag"}
      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "buffers an incomplete packet" do
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        encoder: HaltCodec
      }

      assert {:noreply, %{remaining: "partial"}} =
               HaltProtocol.handle_info({:tcp, self(), "partial"}, socket)

      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "handles every complete packet and buffers the trailing fragment" do
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        assigns: %{packet_observer: self()},
        encoder: HaltCodec
      }

      assert {:noreply, %{remaining: "partial"}} =
               HaltProtocol.handle_info({:tcp, self(), "one\ntwo\npartial"}, socket)

      assert_received {:handled_packet, "one"}
      assert_received {:handled_packet, "two"}
      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "preserves socket changes and clears buffered data with {:ignore, socket}" do
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        remaining: "frag",
        assigns: %{ignore_with_socket: true},
        encoder: HaltCodec
      }

      assert {:noreply, %{remaining: <<>>, assigns: %{ignored: true}}} =
               HaltProtocol.handle_info({:tcp, self(), "ment"}, socket)

      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "does not rearm the transport after handle_message/2 stops" do
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        assigns: %{stop: true},
        encoder: HaltCodec
      }

      assert {:stop, :requested, ^socket} =
               HaltProtocol.handle_info({:tcp, self(), "stop"}, socket)

      refute_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "passes the packet handler halt reason without rearming the transport" do
      socket = %ElvenGard.Network.Socket{
        transport: HaltTransport,
        transport_pid: self(),
        encoder: HaltCodec
      }

      assert {:stop, :requested, %{assigns: %{halted: true}}} =
               HaltProtocol.handle_info({:tcp, self(), "halt:reason\n"}, socket)

      assert_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end
  end

  ## Private/internals helpers

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
