defmodule ElvenGard.Network.SocketTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket
  alias ElvenGard.Network.EndpointHelper
  alias ElvenGard.Network.EchoServer

  @transport :gen_tcp

  setup do
    port = EndpointHelper.get_unused_port_number()
    pid = start_supervised!({EchoServer, port: port})

    [port: port, endpoint: pid]
  end

  describe "new/3" do
    test "create a Socket structure" do
      socket = Socket.new(123, :gen_tcp, MySerialiser)

      assert socket.__struct__ == Socket
      assert is_binary(socket.id)
      assert socket.transport_pid == 123
      assert socket.transport == :gen_tcp
      assert socket.serializer == MySerialiser
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

  describe "recv/3" do
    test "receive messages from the server", %{port: port, endpoint: endpoint} do
      socket = build_socket(port)

      EchoServer.send(endpoint, "recv/3 first test")
      assert Socket.recv(socket) == {:ok, "recv/3 first test"}

      EchoServer.send(endpoint, "recv/3 second test")
      assert Socket.recv(socket, 6) == {:ok, "recv/3"}
      assert Socket.recv(socket, 0) == {:ok, " second test"}

      assert Socket.recv(socket, 0, 0) == {:error, :timeout}
    end
  end

  ## Helpers 

  defp build_socket(port, opts \\ []) do
    connect_opts = [:binary] ++ Keyword.merge([active: false], opts)
    {:ok, socket} = @transport.connect({127, 0, 0, 1}, port, connect_opts)
    %Socket{transport: @transport, transport_pid: socket}
  end
end
