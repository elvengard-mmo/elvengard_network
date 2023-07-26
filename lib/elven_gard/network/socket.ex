defmodule ElvenGard.Network.Socket do
  @moduledoc ~S"""
  Manage a socket

  ## Socket fields

    * `:id` - The string id of the socket
    * `:assigns` - The map of socket assigns, default: `%{}`
    * `:transport` - A [Ranch transport](https://ninenines.eu/docs/en/ranch/2.0/guide/transports/)
    * `:transport_pid` - The pid of the socket's transport process
    * `:remaining` - The remaining bytes after a receive and a packet deserialization
    * `:encoder` - The `ElvenGard.Network.PacketCodec` used to encode packets in `send/2` function
  """

  alias __MODULE__
  alias ElvenGard.Network.UUID

  defstruct id: nil,
            transport: nil,
            transport_pid: nil,
            remaining: <<>>,
            assigns: %{},
            encoder: :unset

  @type t :: %Socket{
          id: String.t(),
          transport: atom,
          transport_pid: pid,
          remaining: bitstring,
          assigns: map,
          encoder: module | :unset
        }

  @doc """
  Create a new structure
  """
  @spec new(pid, atom, module) :: Socket.t()
  def new(transport_pid, transport, encoder) do
    %Socket{
      id: UUID.uuid4(),
      transport_pid: transport_pid,
      transport: transport,
      encoder: encoder
    }
  end

  @doc """
  Send a packet to the client.
  """
  @spec send(Socket.t(), any) :: :ok | {:error, atom}
  def send(%Socket{encoder: :unset} = socket, data) do
    %Socket{transport: transport, transport_pid: transport_pid} = socket
    transport.send(transport_pid, data)
  end

  def send(%Socket{} = socket, message) do
    %Socket{transport: transport, transport_pid: transport_pid, encoder: encoder} = socket
    data = encoder.serialize(message, socket)
    transport.send(transport_pid, data)
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
end
