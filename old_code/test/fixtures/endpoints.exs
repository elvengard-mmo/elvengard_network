defmodule MyApp.EchoEndpoint do
  use GenServer

  @timeout 1_000

  ## Public API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def subscribe(pid) do
    ip = {127, 0, 0, 1}
    port = GenServer.call(pid, :port)
    # ref = make_ref()

    GenServer.cast(pid, {:subscribe, self()})

    {:ok, transport_pid} = :gen_tcp.connect(ip, port, [:binary, active: false], @timeout)
    transport_pid
  end

  def send_message(pid, message, delay \\ 0) do
    GenServer.cast(pid, {:send_message, message, delay, self()})
  end

  def stop(pid) do
    GenServer.call(pid, :stop)
  end

  ## GenServer callbacks

  @impl true
  def init(opts) do
    port = Keyword.get(opts, :port, 0)
    opts = [:binary, packet: :raw, active: true, reuseaddr: true]
    {:ok, socket} = :gen_tcp.listen(port, opts)
    {:ok, %{socket: socket, port: port, subscribers: %{}}}
  end

  @impl true
  def handle_call(:stop, _from, state), do: {:stop, :normal, state}

  def handle_call(:port, _, %{socket: socket, port: 0} = state) do
    {:ok, port} = :inet.port(socket)
    {:reply, port, %{state | port: port}}
  end

  def handle_call(:port, _, %{port: port} = state) do
    {:reply, port, state}
  end

  @impl true
  def handle_cast({:subscribe, pid}, %{socket: socket, subscribers: subs} = state) do
    {:ok, client_socket} = :gen_tcp.accept(socket, @timeout)
    {:noreply, %{state | subscribers: Map.put(subs, client_socket, pid)}}
  end

  def handle_cast({:send_message, message, 0, from}, %{subscribers: subs} = state) do
    do_send_message(subs, from, message)
    {:noreply, state}
  end

  def handle_cast({:send_message, message, delay, from}, state) do
    req = {:send_message, message, 0, from}
    Process.send_after(self(), {:"$gen_cast", req}, delay)
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp, from, data}, %{subscribers: subs} = state) do
    send(subs[from], {:new_message, data})
    {:noreply, state}
  end

  def handle_info({:tcp_closed, from}, %{subscribers: subs} = state) do
    {:noreply, %{state | subscribers: Map.delete(subs, from)}}
  end

  ## Private function

  @doc false
  defp do_send_message(subscribers, from, message) do
    subscribers
    |> Enum.find(fn {_key, val} -> val == from end)
    |> elem(0)
    |> :gen_tcp.send(message)
  end
end
