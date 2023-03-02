defmodule ElvenGard.Network.Socket do
  @moduledoc ~S"""
  Manage a socket

  ## Socket fields

    * `:id` - The string id of the socket
    * `:assigns` - The map of socket assigns, default: `%{}`
    * `:transport` - A [Ranch transport](https://ninenines.eu/docs/en/ranch/2.0/guide/transports/)
    * `:transport_pid` - The pid of the socket's transport process
    * `:serializer` - The serializer for socket messages
  """

  alias __MODULE__
  alias ElvenGard.Network.UUID

  @default_timeout 5_000

  defstruct id: nil,
            transport: nil,
            transport_pid: nil,
            serializer: nil,
            assigns: %{}

  @type t :: %Socket{
          id: String.t(),
          transport: atom,
          transport_pid: pid,
          serializer: module | nil,
          assigns: map
        }

  @doc """
  Create a new structure
  """
  @spec new(pid, atom, module | nil) :: Socket.t()
  def new(transport_pid, transport, serializer \\ nil) do
    %Socket{
      id: UUID.uuid4(),
      transport_pid: transport_pid,
      transport: transport,
      serializer: serializer
    }
  end

  # TODO: Rewrite the doc
  @doc """
  Send a packet to the client.

  If a serializer is found, the function `c:ElvenGard.Network.Codec.encode/2` 
  will be called first with Socket's assigns as the second parameter
  to encode the message.

  ## Examples

      iex> send(socket, "data")
      iex> send(socket, "data", foo: :bar)
  """
  @spec send(Socket.t(), any, keyword) :: :ok | {:error, atom}
  def send(%Socket{} = socket, message, _opts \\ []) do
    send_message(socket, message)
  end

  @doc """
  Receive a packet from the client.

  ...

  ## Examples

      iex> recv(socket)
      iex> recv(socket, 10)
      iex> recv(socket, 0, 10_000)
  """
  @spec recv(Socket.t(), non_neg_integer, timeout) ::
          {:ok, data :: any} | {:error, :closed | :timeout | atom}
  def recv(%Socket{} = socket, length \\ 0, timeout \\ @default_timeout) do
    receive_message(socket, length, timeout)
  end

  @doc """
  Adds key value pairs to socket assigns.

  A single key value pair may be passed, a keyword list or map
  of assigns may be provided to be merged into existing socket
  assigns.

  ## Examples

      iex> assign(socket, :name, "ElvenGard")
      iex> assign(socket, name: "ElvenGard", logo: "🌸")
  """
  @spec assign(Socket.t(), atom, any) :: Socket.t()
  def assign(%Socket{} = socket, key, value) do
    assign(socket, [{key, value}])
  end

  @spec assign(Socket.t(), map | keyword) :: Socket.t()
  def assign(%Socket{} = socket, attrs) when is_map(attrs) or is_list(attrs) do
    %{socket | assigns: Map.merge(socket.assigns, Map.new(attrs))}
  end

  ## Private functions

  @doc false
  defp send_message(socket, message) do
    %Socket{transport: transport, transport_pid: transport_pid} = socket
    transport.send(transport_pid, message)
  end

  @doc false
  defp receive_message(socket, length, timeout) do
    %Socket{transport: transport, transport_pid: transport_pid} = socket
    transport.recv(transport_pid, length, timeout)
  end
end
