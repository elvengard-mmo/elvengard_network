defmodule ElvenGard.Network.Socket do
  @moduledoc ~S"""
  Manage a socket.

  This module provides functionality for managing a socket in the network protocol.
  A socket is a connection between the server and a client. It maintains various
  socket fields, such as the socket ID, socket assigns, transport information,
  and the packet network encoder used for sending data.

  ## Socket fields

  - `:id`: The unique string ID of the socket.
  - `:assigns`: A map of socket assigns, which can be used to store custom data
    associated with the socket. The default value is `%{}`.
  - `:transport`: The [Ranch transport](https://ninenines.eu/docs/en/ranch/2.0/guide/transports/)
    used for the socket.
  - `:transport_pid`: The PID (Process ID) of the socket's transport process.
  - `:remaining`: The remaining bytes after receiving and packet deserialization.
  - `:encoder`: The `ElvenGard.Network.NetworkCodec` module used to encode packets
    in the `send/2` function.

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
  Create a new socket structure.

  This function initializes a new socket with the given `transport_pid`, `transport`,
  and `encoder` module.
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

  This function sends a packet to the client through the socket's transport.
  If the socket's `encoder` is set to `:unset`, the data is sent as is.
  Otherwise, the `encoder` module is used to serialize the data before sending it.

  ## Examples

      iex> ElvenGard.Network.Socket.send(socket, %LoginResponse{status: 200, message: "Welcome!"})
      :ok

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
