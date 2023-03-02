defmodule ElvenGard.Network.EchoServer do
  use GenServer

  def start_link(port: port) do
    GenServer.start_link(__MODULE__, %{socket: nil, client: nil, port: port})
  end

  def send(pid, data) do
    GenServer.cast(pid, {:send, data})
  end

  def init(%{port: port} = state) do
    {:ok, socket} = :gen_tcp.listen(port, [:binary, active: true])
    {:ok, %{state | socket: socket}, {:continue, :listen}}
  end

  def handle_continue(:listen, %{socket: socket} = state) do
    {:ok, client} = :gen_tcp.accept(socket)
    {:noreply, %{state | client: client}}
  end

  def handle_cast({:send, data}, %{client: client} = state) do
    :ok = :gen_tcp.send(client, data)
    {:noreply, state}
  end

  def handle_info({:tcp, socket, data}, state) do
    :ok = :gen_tcp.send(socket, data)
    {:noreply, state}
  end

  def handle_info(_, state), do: {:stop, :normal, state}
end
