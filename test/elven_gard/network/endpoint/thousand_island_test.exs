defmodule ElvenGard.Network.Endpoint.ThousandIslandTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint.Protocol.ThousandIsland, as: Protocol
  alias ElvenGard.Network.Socket

  @tls_certfile Path.expand("../../../fixtures/tls/server_cert.pem", __DIR__)
  @tls_keyfile Path.expand("../../../fixtures/tls/server_key.pem", __DIR__)

  Application.put_env(:elvengard_network, __MODULE__.Endpoint,
    adapter: ElvenGard.Network.Endpoint.Adapters.ThousandIsland,
    adapter_options: [num_acceptors: 2],
    ip: "127.0.0.1",
    listener_name: __MODULE__.Listener,
    port: 0,
    socket_handler: __MODULE__.SocketHandler,
    transport: :tcp,
    transport_options: []
  )

  Application.put_env(:elvengard_network, __MODULE__.SocketHandler,
    network_codec: __MODULE__.Codec,
    packet_handler: __MODULE__.PacketHandler
  )

  Application.put_env(:elvengard_network, __MODULE__.TlsEndpoint,
    adapter: ElvenGard.Network.Endpoint.Adapters.ThousandIsland,
    adapter_options: [num_acceptors: 2],
    ip: "127.0.0.1",
    listener_name: __MODULE__.TlsListener,
    port: 0,
    socket_handler: __MODULE__.SocketHandler,
    transport: :ssl,
    transport_options: [certfile: @tls_certfile, keyfile: @tls_keyfile]
  )

  Application.put_env(:elvengard_network, __MODULE__.ShutdownEndpoint,
    adapter: ElvenGard.Network.Endpoint.Adapters.ThousandIsland,
    adapter_options: [num_acceptors: 1],
    ip: {127, 0, 0, 1},
    listener_name: __MODULE__.ShutdownListener,
    port: 0,
    socket_handler: __MODULE__.ShutdownSocketHandler,
    transport: :tcp,
    transport_options: []
  )

  Application.put_env(:elvengard_network, __MODULE__.ShutdownSocketHandler,
    network_codec: __MODULE__.Codec,
    packet_handler: __MODULE__.PacketHandler
  )

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
    def handle_packet(%Packet{data: "halt"}, socket) do
      {:halt, socket}
    end

    def handle_packet(%Packet{data: data}, socket) do
      %Socket{assigns: %{observer: observer}} = socket
      send(observer, {:handle_packet, data})
      :ok = Socket.send(socket, %Packet{data: "ack:" <> data})
      {:cont, socket}
    end
  end

  defmodule Endpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule TlsEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule ShutdownEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule SocketHandler do
    use ElvenGard.Network.SocketHandler

    ## SocketHandler callbacks

    @impl true
    def handle_init(%Socket{} = socket) do
      :ok = Socket.setopts(socket, packet: 1)
      {:ok, Socket.assign(socket, :connection_state, :waiting_for_observer)}
    end

    @impl true
    def handle_message(message, %{assigns: %{connection_state: :waiting_for_observer}} = socket) do
      observer = :erlang.binary_to_term(message)
      :ok = Socket.setopts(socket, packet: 0)
      send(observer, :handle_init)

      {:ignore, Socket.assign(socket, %{connection_state: :connected, observer: observer})}
    end

    def handle_message(_message, %{assigns: %{ignore: true}}), do: :ignore

    def handle_message("invalid", _socket), do: :invalid

    def handle_message("stop", socket) do
      {:stop, :requested, socket}
    end

    def handle_message(_message, socket), do: {:ok, socket}

    @impl true
    def handle_halt(reason, %{assigns: %{observer: observer}} = socket) do
      send(observer, {:handle_halt, reason})
      {:ok, socket}
    end

    def handle_halt(_reason, socket), do: {:ok, socket}
  end

  defmodule TimeoutSocketHandler do
    use ElvenGard.Network.SocketHandler

    ## SocketHandler callbacks

    @impl true
    def handle_init(socket), do: {:ok, socket, 250}
  end

  defmodule StopSocketHandler do
    use ElvenGard.Network.SocketHandler

    ## SocketHandler callbacks

    @impl true
    def handle_init(socket), do: {:stop, :requested, socket}

    @impl true
    def handle_halt(reason, socket) do
      send(self(), {:init_stop_halt, reason})
      {:ok, socket}
    end
  end

  defmodule InvalidSocketHandler do
    use ElvenGard.Network.SocketHandler

    ## SocketHandler callbacks

    @impl true
    def handle_init(_socket), do: :invalid
  end

  defmodule ShutdownSocketHandler do
    use ElvenGard.Network.SocketHandler

    ## SocketHandler callbacks

    @impl true
    def handle_init(socket) do
      send(Process.whereis(__MODULE__.Observer), :shutdown_init)
      {:ok, socket}
    end

    @impl true
    def handle_halt(reason, socket) do
      send(Process.whereis(__MODULE__.Observer), {:shutdown_halt, reason})
      {:ok, socket}
    end
  end

  setup_all do
    start_supervised!(Endpoint)
    start_supervised!(TlsEndpoint)
    :ok
  end

  test "reports the bound address and OS-assigned port" do
    assert Endpoint.get_addr() == "127.0.0.1"
    assert Endpoint.get_port() > 0
  end

  test "runs the same init, decoding and sending pipeline as Ranch" do
    socket = connect()
    assert_receive :handle_init

    :ok = :gen_tcp.send(socket, "fragment")
    refute_receive {:handle_packet, _data}, 25

    :ok = :gen_tcp.send(socket, "ed\n")
    assert_receive {:handle_packet, "fragmented"}
    assert {:ok, "ack:fragmented\n"} = :gen_tcp.recv(socket, 0)

    :ok = :gen_tcp.close(socket)
    assert_receive {:handle_halt, :closed}
  end

  test "runs the complete fragmented packet pipeline through TLS" do
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

  test "closes active connections and runs halt cleanup when the listener shuts down" do
    Process.register(self(), ShutdownSocketHandler.Observer)
    child_spec = ShutdownEndpoint.child_spec([])
    start_supervised!(child_spec)

    {:ok, socket} =
      :gen_tcp.connect(
        {127, 0, 0, 1},
        ShutdownEndpoint.get_port(),
        [:binary, active: false]
      )

    assert_receive :shutdown_init
    :ok = stop_supervised(child_spec.id)

    assert_receive {:shutdown_halt, :closed}
    assert {:error, :closed} = :gen_tcp.recv(socket, 0)
    refute_received {:shutdown_halt, :closed}
  end

  test "closes and runs halt cleanup when the packet handler halts" do
    socket = connect()
    assert_receive :handle_init

    :ok = :gen_tcp.send(socket, "halt\n")

    assert_receive {:handle_halt, :normal}
    assert {:error, :closed} = :gen_tcp.recv(socket, 0)
    refute_received {:handle_halt, :normal}
  end

  test "preserves a stop reason returned by the socket handler" do
    socket = connect()
    assert_receive :handle_init

    :ok = :gen_tcp.send(socket, "stop")

    assert_receive {:handle_halt, :requested}
    assert {:error, :closed} = :gen_tcp.recv(socket, 0)
    refute_received {:handle_halt, :requested}
  end

  test "translates socket handler init timeouts and stops" do
    assert {:continue, %Protocol.State{}, 250} =
             Protocol.handle_connection(
               :transport_socket,
               runtime_options(TimeoutSocketHandler)
             )

    assert {:close, %Protocol.State{halt_reason: :requested} = state} =
             Protocol.handle_connection(
               :transport_socket,
               runtime_options(StopSocketHandler)
             )

    refute_received {:init_stop_halt, :requested}
    assert :ok = Protocol.handle_close(:transport_socket, state)
    assert_received {:init_stop_halt, :requested}
    refute_received {:init_stop_halt, :requested}
  end

  test "rejects an invalid socket handler init result" do
    assert_raise RuntimeError, ~r/handle_init\/1 must return/, fn ->
      Protocol.handle_connection(
        :transport_socket,
        runtime_options(InvalidSocketHandler)
      )
    end
  end

  test "normalizes errors, timeouts and listener shutdowns" do
    state = runtime_state()

    assert :ok = Protocol.handle_error(:econnreset, :current_transport_socket, state)
    assert_receive {:handle_halt, {:error, :econnreset}}

    assert :ok = Protocol.handle_timeout(:current_transport_socket, state)
    assert_receive {:handle_halt, :timeout}

    assert :ok = Protocol.handle_shutdown(:current_transport_socket, state)
    assert_receive {:handle_halt, :closed}
  end

  test "clears buffered data when raw message handling ignores it" do
    state = runtime_state(remaining: "stale", assigns: %{ignore: true})

    assert {:continue, %Protocol.State{socket: %Socket{remaining: <<>>}}} =
             Protocol.handle_data("data", :current_transport_socket, state)
  end

  test "rejects an invalid raw message result" do
    assert_raise RuntimeError, ~r/invalid return value for handle_message\/2/, fn ->
      Protocol.handle_data("invalid", :current_transport_socket, runtime_state())
    end
  end

  ## Private function

  defp runtime_options(socket_handler) do
    [
      socket_handler: socket_handler,
      network_codec: Codec,
      packet_handler: PacketHandler
    ]
  end

  defp runtime_state(socket_attrs \\ []) do
    assigns =
      %{connection_state: :connected, observer: self()}
      |> Map.merge(Keyword.get(socket_attrs, :assigns, %{}))

    socket = %Socket{
      id: "runtime-test",
      adapter: ElvenGard.Network.Socket.Adapters.ThousandIsland,
      adapter_state: :previous_transport_socket,
      remaining: Keyword.get(socket_attrs, :remaining, <<>>),
      assigns: assigns,
      encoder: Codec
    }

    %Protocol.State{
      socket: socket,
      socket_handler: SocketHandler,
      packet_handler: PacketHandler
    }
  end

  defp connect() do
    {:ok, socket} =
      :gen_tcp.connect(
        {127, 0, 0, 1},
        Endpoint.get_port(),
        [:binary, active: false, packet: 0]
      )

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

  defp serialized_self() do
    data = :erlang.term_to_binary(self())
    <<byte_size(data)::8, data::binary>>
  end
end
