defmodule ElvenGard.Network.SocketTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.EchoServer
  alias ElvenGard.Network.EndpointHelper
  alias ElvenGard.Network.Socket
  alias ElvenGard.Network.Socket.Adapters.Ranch

  @transport :gen_tcp

  defmodule TransportMock do
    import Kernel, except: [send: 2]

    @behaviour ElvenGard.Network.Socket.Adapter

    @impl true
    def new(options) do
      owner = Keyword.fetch!(options, :owner)
      Kernel.send(owner, :adapter_initialized)
      owner
    end

    @impl true
    def send(_owner, _data), do: :ok

    @impl true
    def setopts(owner, opts) do
      Kernel.send(owner, {:setopts, opts})
      :ok
    end

    @impl true
    def close(owner) do
      Kernel.send(owner, :close)
      :ok
    end
  end

  setup do
    port = EndpointHelper.get_unused_port_number()
    pid = start_supervised!({EchoServer, port: port})

    [port: port, endpoint: pid]
  end

  describe "new/3" do
    test "create a Socket structure" do
      socket = Socket.new(TransportMock, [owner: self()], MyApp.Encoder)

      assert socket.__struct__ == Socket
      assert is_binary(socket.id)
      assert socket.adapter == TransportMock
      assert socket.adapter_state == self()
      assert socket.remaining == <<>>
      assert socket.encoder == MyApp.Encoder
      assert_received :adapter_initialized
    end
  end

  describe "assign/3" do
    test "assigns to socket" do
      socket = %Socket{}
      assert socket.assigns[:foo] == nil
      socket = Socket.assign(socket, :foo, :bar)
      assert socket.assigns[:foo] == :bar
    end
  end

  describe "assign/2" do
    test "assigns a map socket" do
      socket = %Socket{}
      assert socket.assigns[:foo] == nil
      socket = Socket.assign(socket, %{foo: :bar, abc: :def})
      assert socket.assigns[:foo] == :bar
      assert socket.assigns[:abc] == :def
    end

    test "merges if values exist" do
      socket = %Socket{}
      socket = Socket.assign(socket, %{foo: :bar, abc: :def})
      socket = Socket.assign(socket, %{foo: :baz})
      assert socket.assigns[:foo] == :baz
      assert socket.assigns[:abc] == :def
    end

    test "merges keyword lists" do
      socket = %Socket{}
      socket = Socket.assign(socket, %{foo: :bar, abc: :def})
      socket = Socket.assign(socket, foo: :baz)
      assert socket.assigns[:foo] == :baz
      assert socket.assigns[:abc] == :def
    end
  end

  describe "send/2" do
    test "send the message to the server", %{port: port} do
      socket = build_socket(port, active: true)

      assert :ok = Socket.send(socket, "send/2 first test")
      assert_receive {:tcp, _, "send/2 first test"}
    end
  end

  describe "setopts/2" do
    test "delegates to the socket adapter" do
      socket = Socket.new(TransportMock, [owner: self()], MyApp.Encoder)

      assert :ok = Socket.setopts(socket, active: :once)
      assert_received {:setopts, [active: :once]}
    end
  end

  describe "close/1" do
    test "delegates to the socket adapter" do
      socket = Socket.new(TransportMock, [owner: self()], MyApp.Encoder)

      assert :ok = Socket.close(socket)
      assert_received :close
    end
  end

  ## Helpers

  defp build_socket(port, opts) do
    connect_opts = [:binary] ++ Keyword.merge([active: false], opts)
    {:ok, socket} = @transport.connect({127, 0, 0, 1}, port, connect_opts)

    Socket.new(
      Ranch,
      [transport: @transport, socket: socket],
      ElvenGard.Network.DummyEncoder
    )
  end
end
