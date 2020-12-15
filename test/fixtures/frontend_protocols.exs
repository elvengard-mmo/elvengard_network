defmodule MyApp.FrontendProtocol do
  use ElvenGard.Endpoint.Protocol

  @impl true
  def handle_connection(state) do
    %{transport: transport, transport_pid: transport_pid} = state
    :ok = transport.setopts(transport_pid, active: :once, packet: :raw, reuseaddr: true)
    transport.send(transport_pid, "init done!")
    {:ok, state}
  end

  @impl true
  def handle_halt(reason, state) do
    %{transport: transport, transport_pid: transport_pid} = state
    transport.send(transport_pid, "halt #{inspect(reason)}")
    {:ok, state}
  end
end
