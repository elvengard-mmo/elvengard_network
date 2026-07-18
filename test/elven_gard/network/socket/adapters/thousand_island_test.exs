defmodule ElvenGard.Network.Socket.Adapters.ThousandIslandTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Socket.Adapters.ThousandIsland, as: Adapter
  alias ThousandIsland.Socket, as: ThousandIslandSocket

  defmodule TransportMock do
    import Kernel, except: [send: 2]

    ## Public API

    def send(owner, data) do
      Kernel.send(owner, {:transport_send, data})
      :ok
    end

    def setopts(owner, opts) do
      Kernel.send(owner, {:transport_opts, opts})
      :ok
    end

    def close(owner) do
      Kernel.send(owner, :transport_closed)
      :ok
    end
  end

  test "new/1 owns the Thousand Island socket as adapter state" do
    socket = thousand_island_socket()
    assert Adapter.new(socket: socket) == socket
  end

  test "send/2 delegates to the Thousand Island socket" do
    assert :ok = Adapter.send(thousand_island_socket(), ["da", "ta"])
    assert_received {:transport_send, ["da", "ta"]}
  end

  test "setopts/2 delegates to the Thousand Island socket" do
    assert :ok = Adapter.setopts(thousand_island_socket(), packet: :line)
    assert_received {:transport_opts, packet: :line}
  end

  test "close/1 delegates to the Thousand Island socket" do
    assert :ok = Adapter.close(thousand_island_socket())
    assert_received :transport_closed
  end

  ## Private function

  defp thousand_island_socket() do
    span = ThousandIsland.Telemetry.start_span(:connection, %{}, %{handler: __MODULE__})

    %ThousandIslandSocket{
      socket: self(),
      transport_module: TransportMock,
      read_timeout: :infinity,
      silent_terminate_on_error: false,
      span: span
    }
  end
end
