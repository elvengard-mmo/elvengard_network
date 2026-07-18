defmodule ElvenGard.Network.Socket.Adapters.RanchTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket.Adapters.Ranch

  defmodule TransportMock do
    import Kernel, except: [send: 2]

    def send(owner, data) do
      Kernel.send(owner, {:send, data})
      :ok
    end

    def setopts(owner, opts) do
      Kernel.send(owner, {:setopts, opts})
      :ok
    end

    def close(owner) do
      Kernel.send(owner, :close)
      :ok
    end
  end

  test "new/1 creates the adapter state" do
    owner = self()

    assert {TransportMock, ^owner} =
             Ranch.new(transport: TransportMock, socket: owner)
  end

  test "send/2 delegates to the Ranch transport" do
    assert :ok = Ranch.send({TransportMock, self()}, "data")
    assert_received {:send, "data"}
  end

  test "setopts/2 delegates to the Ranch transport" do
    assert :ok = Ranch.setopts({TransportMock, self()}, active: :once)
    assert_received {:setopts, [active: :once]}
  end

  test "close/1 delegates to the Ranch transport" do
    assert :ok = Ranch.close({TransportMock, self()})
    assert_received :close
  end
end
