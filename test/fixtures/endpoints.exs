defmodule MyApp.EchoEndpoint do
  use GenServer

  @timeout 1_000

  ## Public API

  def start_link(port) do
    GenServer.start_link(__MODULE__, port)
  end

  def subscribe(pid) do
    ip = {127, 0, 0, 1}
    port = GenServer.call(pid, :port)

    GenServer.cast(pid, {:subscribe, self()})

    {:ok, transport_pid} = :gen_tcp.connect(ip, port, [:binary, active: true], @timeout)
    transport_pid
  end

  def stop(pid) do
    GenServer.call(pid, :stop)
  end

  ## GenServer callbacks

  @impl true
  def init(port) when is_integer(port) do
    opts = [:binary, packet: :raw, active: true, reuseaddr: true]
    {:ok, socket} = :gen_tcp.listen(port, opts)
    {:ok, %{socket: socket, port: port, subscribers: %{}}}
  end

  @impl true
  def handle_call(:stop, _from, state), do: {:stop, :normal, state}

  def handle_call(:port, _, %{port: port} = state) do
    {:reply, port, state}
  end

  @impl true
  def handle_cast({:subscribe, pid}, %{socket: socket, subscribers: subs} = state) do
    {:ok, client_socket} = :gen_tcp.accept(socket, @timeout)
    {:noreply, %{state | subscribers: Map.put(subs, client_socket, pid)}}
  end

  @impl true
  def handle_info({:tcp, from, data}, %{subscribers: subs} = state) do
    send(subs[from], {:new_message, data})
    {:noreply, state}
  end

  def handle_info({:tcp_closed, from}, %{subscribers: subs} = state) do
    {:noreply, %{state | subscribers: Map.delete(subs, from)}}
  end
end
