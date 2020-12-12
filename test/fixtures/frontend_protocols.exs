defmodule MyApp.FrontendProtocol do
  use ElvenGard.Endpoint.Protocol

  @impl true
  def handle_init(state) do
    %{transport: transport, transport_pid: transport_pid} = state
    :ok = transport.setopts(transport_pid, active: :once, packet: :raw, reuseaddr: true)

    transport.send(transport_pid, "init done!")
    {:ok, state}
  end
end
