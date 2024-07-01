defmodule ElvenGard.Network.ProtocolTest do
  use ExUnit.Case, async: true

  Application.put_env(:elvengard_network, __MODULE__.MyEndpoint,
    listener_name: :my_endpoint,
    protocol: __MODULE__.MyProtocol,
    transport_opts: [ip: {127, 0, 0, 1}, port: 0]
  )

  Application.put_env(:elvengard_network, __MODULE__.MyProtocol, [])

  defmodule MyEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  defmodule MyProtocol do
    use ElvenGard.Network.Endpoint.Protocol

    import ElvenGard.Network.Socket, only: [assign: 3]

    alias ElvenGard.Network.Socket

    @timeout 100

    @impl true
    def handle_connection(%Socket{transport: transport, transport_pid: transport_pid} = socket) do
      # the first packet (link's pid) is prefixed with the packet length (cf. serialized_self/0)
      transport.setopts(transport_pid, packet: 1)
      {:ok, bin} = transport.recv(transport_pid, 0, @timeout)
      transport.setopts(transport_pid, packet: 0)

      link = :erlang.binary_to_term(bin)

      send(link, :handle_connection)
      {:ok, assign(socket, :link, link)}
    end

    @impl true
    def handle_data(message, socket) do
      send(socket.assigns[:link], {:handle_data, message})
      {:skip, socket}
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

  describe "c:handle_connection/1" do
    test "is called" do
      _socket = connect()
      assert_receive :handle_connection
    end
  end

  describe "c:handle_data/1" do
    test "is called" do
      socket = connect()
      send_data(socket, "test c:handle_data/1")

      assert_receive {:handle_data, "test c:handle_data/1"}
    end
  end

  describe "c:handle_halt/1" do
    test "is called" do
      socket = connect()
      close(socket)

      assert_receive {:handle_halt, :tcp_closed}
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
