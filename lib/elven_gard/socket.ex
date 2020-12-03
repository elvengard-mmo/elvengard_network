defmodule ElvenGard.Socket do
  @moduledoc """
  Manage a socket

  ## Socket fields

    * `:id` - The string id of the socket
    * `:transport` - A [Ranch transport](https://ninenines.eu/docs/en/ranch/2.0/guide/transports/)
    * `:transport_pid` - The pid of the socket's transport process
    * `:serializer` - The serializer for socket messages, default: `nil`
    * `:frontend_pid` - The map of socket assigns, default: `nil`
    * `:assigns` - The map of socket assigns, default: `%{}`
  """

  alias ElvenGard.{Socket, UUID}

  # @enforce_keys [:id, :socket, :transport]

  defstruct id: nil,
            transport: nil,
            transport_pid: nil,
            serializer: nil,
            frontend_pid: nil,
            assigns: %{}

  @type t :: %Socket{
          id: String.t(),
          transport: atom(),
          transport_pid: pid(),
          serializer: module() | nil,
          frontend_pid: pid() | nil,
          assigns: map()
        }

  @doc """
  Create a new structure
  """
  @spec new(pid(), atom(), module() | nil, pid() | nil) :: Socket.t()
  def new(transport_pid, transport, serializer \\ nil, frontend_pid \\ nil) do
    %Socket{
      id: UUID.uuid4(),
      transport_pid: transport_pid,
      transport: transport,
      serializer: serializer,
      frontend_pid: frontend_pid
    }
  end

  @doc """
  Send a packet to the client.

  If a serializer is found, the function `c:ElvenGard.Codec.encode/2` 
  will be called first to encode the message.  

  The message will then be sent directly to the client if no 
  frontend is found or, otherwise, the corresponding frontend 
  will be notified via `ElvenGard.Frontend.send/2`.

  ## Examples

      iex> send(socket, "data")
  """
  @spec send(Socket.t(), any()) :: :ok | {:error, atom()}
  def send(%Socket{serializer: serializer} = socket, message)
      when not is_nil(serializer) do
    encoded_msg = serializer.encode!(message, socket.assigns)
    Socket.send(%Socket{socket | serializer: nil}, encoded_msg)
  end

  def send(%Socket{frontend_pid: frontend_pid} = socket, message)
      when is_nil(frontend_pid)
      when frontend_pid == self() do
    %Socket{transport_pid: transport_pid, transport: transport} = socket
    transport.send(transport_pid, message)
  end

  def send(%Socket{} = socket, message) do
    frontend_mod().send(socket, message)
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
  @spec assign(Socket.t(), atom(), any()) :: Socket.t()
  def assign(%Socket{} = socket, key, value) do
    assign(socket, [{key, value}])
  end

  @spec assign(Socket.t(), map() | keyword()) :: Socket.t()
  def assign(%Socket{} = socket, attrs) when is_map(attrs) or is_list(attrs) do
    %{socket | assigns: Map.merge(socket.assigns, Map.new(attrs))}
  end

  ## Private functions

  @doc false
  defp frontend_mod() do
    Application.get_env(:elven_gard, :frontend_mod, ElvenGard.Frontend)
  end
end
