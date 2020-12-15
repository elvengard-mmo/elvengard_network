defmodule MyApp.FrontendProtocol do
  use ElvenGard.Endpoint.Protocol

  @impl true
  def handle_init(socket) do
    %{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :raw, reuseaddr: true)
    transport.send(transport_pid, "init done!")
    {:ok, socket}
  end

  @impl true
  def handle_message("fpid", socket) do
    %{
      transport: transport,
      transport_pid: transport_pid,
      frontend_pid: frontend_pid
    } = socket

    transport.send(transport_pid, "fpid #{inspect(frontend_pid)}")
    {:ignore, Map.put(socket, :debug, true)}
  end

  @impl true
  def handle_halt(:tcp_closed, socket) do
    {:ok, socket}
  end
end
