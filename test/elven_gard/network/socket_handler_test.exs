defmodule ElvenGard.Network.SocketHandlerTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket

  defmodule DefaultHandler do
    use ElvenGard.Network.SocketHandler
  end

  defmodule CustomHandler do
    use ElvenGard.Network.SocketHandler

    @impl true
    def handle_init(%Socket{} = socket) do
      {:ok, Socket.assign(socket, :initialized, true)}
    end

    @impl true
    def handle_message(message, %Socket{} = socket) do
      {:ignore, Socket.assign(socket, :message, message)}
    end

    @impl true
    def handle_halt(reason, %Socket{} = socket) do
      {:ok, reason, Socket.assign(socket, :halted, true)}
    end
  end

  test "provides callbacks that keep the socket unchanged by default" do
    socket = %Socket{}

    assert {:ok, ^socket} = DefaultHandler.handle_init(socket)
    assert {:ok, ^socket} = DefaultHandler.handle_message("data", socket)
    assert {:ok, ^socket} = DefaultHandler.handle_halt(:normal, socket)
  end

  test "allows every callback to be overridden" do
    socket = %Socket{}

    assert {:ok, %Socket{assigns: %{initialized: true}}} =
             CustomHandler.handle_init(socket)

    assert {:ignore, %Socket{assigns: %{message: "data"}}} =
             CustomHandler.handle_message("data", socket)

    assert {:ok, :requested, %Socket{assigns: %{halted: true}}} =
             CustomHandler.handle_halt(:requested, socket)
  end
end
