defmodule ElvenGard.Network.Endpoint.RanchTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint.Protocol.Ranch
  alias ElvenGard.Network.Socket
  alias ElvenGard.Network.Socket.Adapters.Ranch, as: RanchAdapter

  @tls_certfile Path.expand("../../../fixtures/tls/server_cert.pem", __DIR__)
  @tls_keyfile Path.expand("../../../fixtures/tls/server_key.pem", __DIR__)

  Application.put_env(:elvengard_network, __MODULE__.MyEndpoint,
    adapter: ElvenGard.Network.Endpoint.Adapters.Ranch,
    ip: {127, 0, 0, 1},
    listener_name: :my_endpoint,
    port: 0,
    socket_handler: __MODULE__.MySocketHandler,
    transport: :tcp
  )

  Application.put_env(:elvengard_network, __MODULE__.MySocketHandler,
    network_codec: __MODULE__.HaltCodec,
    packet_handler: __MODULE__.HaltHandler
  )

  Application.put_env(:elvengard_network, __MODULE__.InitStopEndpoint,
    adapter: ElvenGard.Network.Endpoint.Adapters.Ranch,
    ip: {127, 0, 0, 1},
    listener_name: :ranch_init_stop_endpoint,
    port: 0,
    socket_handler: __MODULE__.InitStopSocketHandler,
    transport: :tcp
  )

  Application.put_env(:elvengard_network, __MODULE__.InitStopSocketHandler,
    network_codec: __MODULE__.HaltCodec,
    packet_handler: __MODULE__.HaltHandler
  )

  Application.put_env(:elvengard_network, __MODULE__.TlsEndpoint,
    adapter: ElvenGard.Network.Endpoint.Adapters.Ranch,
    ip: "127.0.0.1",
    listener_name: :ranch_tls_endpoint,
    port: 0,
    socket_handler: __MODULE__.MySocketHandler,
    transport: :ssl,
    transport_options: [certfile: @tls_certfile, keyfile: @tls_keyfile]
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

    def handle_packet(%HaltPacket{data: data}, socket) do
      %Socket{assigns: assigns} = socket

      case assigns do
        %{link: link} ->
          send(link, {:handle_packet, data})
          :ok = Socket.send(socket, ["ack:", data, "\n"])
          {:cont, socket}

        _ ->
          {:cont, socket}
      end
    end
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

  defmodule HaltSocketHandler do
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
    def handle_halt(reason, socket) do
      send(self(), {:socket_handler_halt, reason})
      {:ok, socket}
    end
  end

  defmodule MyEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule InitStopEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule TlsEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule InitStopSocketHandler do
    use ElvenGard.Network.SocketHandler

    ## SocketHandler callbacks

    @impl true
    def handle_init(socket) do
      send(Process.whereis(__MODULE__.Observer), :init_stop)
      {:stop, :requested, socket}
    end

    @impl true
    def handle_halt(reason, socket) do
      send(Process.whereis(__MODULE__.Observer), {:init_stop_halt, reason})
      {:ok, socket}
    end
  end

  defmodule MySocketHandler do
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
      {:ok, socket}
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
    start_supervised!(InitStopEndpoint)
    start_supervised!(TlsEndpoint)
    :ok
  end

  describe "c:handle_init/1" do
    test "is called" do
      _socket = connect()
      assert_receive :handle_init
    end

    test "closes and runs halt cleanup when initialization stops" do
      Process.register(self(), InitStopSocketHandler.Observer)

      {:ok, socket} =
        :gen_tcp.connect(
          {127, 0, 0, 1},
          InitStopEndpoint.get_port(),
          [:binary, active: false]
        )

      assert_receive :init_stop
      assert_receive {:init_stop_halt, :requested}
      assert {:error, :closed} = :gen_tcp.recv(socket, 0)
      refute_received {:init_stop_halt, :requested}
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

      assert_receive {:handle_halt, :closed}
    end
  end

  describe "TLS integration" do
    test "runs the complete fragmented packet pipeline" do
      socket = connect_tls()
      assert_receive :handle_init

      :ok = :ssl.send(socket, "fragment")
      refute_receive {:handle_packet, _data}, 25

      :ok = :ssl.send(socket, "ed\n")
      assert_receive {:handle_packet, "fragmented"}
      assert {:ok, "ack:fragmented\n"} = :ssl.recv(socket, 0)

      :ok = :ssl.close(socket)
      assert_receive {:handle_halt, :closed}
    end
  end

  describe "message processing" do
    test "receives the socket returned by the packet handler" do
      state = halt_state()

      assert {:stop, :normal, %Ranch.State{socket: %Socket{assigns: %{halted: true}}}} =
               Ranch.handle_info({:tcp, self(), "halt\n"}, state)

      assert_received {:socket_handler_halt, :normal}
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

    test "closes and runs halt cleanup after handle_message/2 stops" do
      state = halt_state(assigns: %{stop: true})

      assert {:stop, :normal, ^state} =
               Ranch.handle_info({:tcp, self(), "stop"}, state)

      assert_received {:socket_handler_halt, :requested}
      assert_received :transport_closed
      refute_received {:socket_handler_halt, :requested}
      refute_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end

    test "passes the packet handler halt reason without rearming the transport" do
      state = halt_state()

      assert {:stop, :normal, %Ranch.State{socket: %Socket{assigns: %{halted: true}}}} =
               Ranch.handle_info({:tcp, self(), "halt:reason\n"}, state)

      assert_received {:socket_handler_halt, :requested}
      assert_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end

    test "processes data received through SSL" do
      state = halt_state()

      assert {:noreply, %Ranch.State{socket: %Socket{remaining: "partial"}}} =
               Ranch.handle_info({:ssl, self(), "partial"}, state)

      assert_received {:transport_opts, active: :once}
      refute_received :transport_closed
    end

    test "normalizes a closed SSL connection" do
      state = halt_state()

      assert {:stop, :normal, ^state} =
               Ranch.handle_info({:ssl_closed, self()}, state)

      assert_received {:socket_handler_halt, :closed}
      assert_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end

    test "normalizes transport errors" do
      state = halt_state()

      assert {:stop, :normal, ^state} =
               Ranch.handle_info({:tcp_error, self(), :econnreset}, state)

      assert_received {:socket_handler_halt, {:error, :econnreset}}
      assert_received :transport_closed
      refute_received {:transport_opts, active: :once}
    end
  end

  ## Private function

  defp halt_state(attrs \\ []) do
    socket =
      Socket.new(
        RanchAdapter,
        [transport: HaltTransport, socket: self()],
        HaltCodec
      )

    %Ranch.State{
      socket: struct!(socket, attrs),
      socket_handler: HaltSocketHandler,
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

  defp connect_tls() do
    {:ok, socket} =
      :ssl.connect(
        {127, 0, 0, 1},
        TlsEndpoint.get_port(),
        [:binary, active: false, packet: 0, verify: :verify_none]
      )

    :ok = :ssl.send(socket, serialized_self())
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
