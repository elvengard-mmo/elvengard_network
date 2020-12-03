Code.require_file("../fixtures/endpoints.exs", __DIR__)
Code.require_file("../fixtures/serializers.exs", __DIR__)

defmodule ElvenGard.SocketTest do
  use ExUnit.Case, async: true

  import ElvenGard.Socket, only: [assign: 2, assign: 3]
  import Mox

  setup_all do
    %{server_pid: start_supervised!({MyApp.EchoEndpoint, 4545})}
  end

  # Make sure mocks are verified when the test exits
  setup :verify_on_exit!

  ## new

  describe "new/2" do
    test "create a socket" do
      socket = ElvenGard.Socket.new(:foo, :bar)

      assert is_binary(socket.id)
      assert socket.transport_pid == :foo
      assert socket.transport == :bar
      assert socket.serializer == nil
      assert socket.frontend_pid == nil
    end
  end

  describe "new/3" do
    test "create a socket with a serializer" do
      socket = ElvenGard.Socket.new(:foo, :bar, :abc)

      assert is_binary(socket.id)
      assert socket.transport_pid == :foo
      assert socket.transport == :bar
      assert socket.serializer == :abc
      assert socket.frontend_pid == nil
    end
  end

  describe "new/4" do
    test "create a socket with a frontend pid" do
      socket = ElvenGard.Socket.new(:foo, :bar, :abc, :baz)

      assert is_binary(socket.id)
      assert socket.transport_pid == :foo
      assert socket.transport == :bar
      assert socket.serializer == :abc
      assert socket.frontend_pid == :baz
    end
  end

  ## assign

  describe "assign/3" do
    test "assigns to socket" do
      socket = %ElvenGard.Socket{}
      assert socket.assigns[:foo] == nil
      socket = assign(socket, :foo, :bar)
      assert socket.assigns[:foo] == :bar
    end
  end

  describe "assign/2" do
    test "assigns a map socket" do
      socket = %ElvenGard.Socket{}
      assert socket.assigns[:foo] == nil
      socket = assign(socket, %{foo: :bar, abc: :def})
      assert socket.assigns[:foo] == :bar
      assert socket.assigns[:abc] == :def
    end

    test "merges if values exist" do
      socket = %ElvenGard.Socket{}
      socket = assign(socket, %{foo: :bar, abc: :def})
      socket = assign(socket, %{foo: :baz})
      assert socket.assigns[:foo] == :baz
      assert socket.assigns[:abc] == :def
    end

    test "merges keyword lists" do
      socket = %ElvenGard.Socket{}
      socket = assign(socket, %{foo: :bar, abc: :def})
      socket = assign(socket, foo: :baz)
      assert socket.assigns[:foo] == :baz
      assert socket.assigns[:abc] == :def
    end
  end

  ## send

  describe "send/2" do
    test "can send a message", %{server_pid: server_pid} do
      socket = %ElvenGard.Socket{
        transport_pid: MyApp.EchoEndpoint.subscribe(server_pid),
        transport: :gen_tcp
      }

      assert ElvenGard.Socket.send(socket, "message") == :ok
      assert_receive {:new_message, "message"}
    end

    test "can use a serializer", %{server_pid: server_pid} do
      socket = %ElvenGard.Socket{
        transport_pid: MyApp.EchoEndpoint.subscribe(server_pid),
        transport: :gen_tcp,
        serializer: MyApp.LFSerializer
      }

      assert ElvenGard.Socket.send(socket, "message") == :ok
      assert_receive {:new_message, "message\n"}
    end

    test "directly send the packet if self is the frontend", %{server_pid: server_pid} do
      socket = %ElvenGard.Socket{
        transport_pid: MyApp.EchoEndpoint.subscribe(server_pid),
        transport: :gen_tcp,
        frontend_pid: self()
      }

      assert ElvenGard.Socket.send(socket, "message") == :ok
      assert_receive {:new_message, "message"}
    end

    test "can delegate a packet to a frontend", %{server_pid: server_pid} do
      expect(ElvenGard.FrontendMock, :send, fn socket, msg ->
        # Notify the Frontend
        send(socket.frontend_pid, {:send_to, socket, msg})

        # Frontend must forward the given packet to the Socket
        patched_socket = %ElvenGard.Socket{socket | frontend_pid: nil}
        ElvenGard.Socket.send(patched_socket, msg)
      end)

      frontend_pid = self()

      task =
        Task.async(fn ->
          # Allow mocks inside task only for Elixir < 1.8.0
          # allow(ElvenGard.FrontendMock, frontend_pid, self())

          socket = %ElvenGard.Socket{
            transport_pid: MyApp.EchoEndpoint.subscribe(server_pid),
            transport: :gen_tcp,
            frontend_pid: frontend_pid
          }

          assert ElvenGard.Socket.send(socket, "message") == :ok
          assert_receive {:new_message, "message"}

          socket
        end)

      socket = Task.await(task)
      assert_receive {:send_to, ^socket, "message"}
    end
  end
end
