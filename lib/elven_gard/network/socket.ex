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
  - `:adapter`: The module implementing `ElvenGard.Network.Socket.Adapter`.
  - `:adapter_state`: The opaque connection state managed exclusively by the adapter.
  - `:remaining`: The remaining bytes after receiving and packet deserialization.
  - `:encoder`: The `ElvenGard.Network.NetworkCodec` module used to encode packets
    in the `send/2` function.

  """

  alias __MODULE__
  alias __MODULE__.Adapter
  alias ElvenGard.Network.UUID

  defstruct id: nil,
            adapter: nil,
            adapter_state: nil,
            remaining: <<>>,
            assigns: %{},
            encoder: :unset

  @type t :: %Socket{
          id: String.t(),
          adapter: module(),
          adapter_state: Adapter.state(),
          remaining: binary(),
          assigns: map(),
          encoder: module() | :unset
        }

  @doc """
  Create a new socket structure.

  This function initializes a new socket with the given `adapter`, the keyword
  options used by the adapter to create its opaque state, and the `encoder` module.
  """
  @spec new(module(), Adapter.options(), module()) :: Socket.t()
  def new(adapter, adapter_options, encoder) do
    adapter_state = adapter.new(adapter_options)

    %Socket{
      id: UUID.uuid4(),
      adapter: adapter,
      adapter_state: adapter_state,
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
  @spec send(Socket.t(), struct() | iodata()) :: :ok | {:error, term()}
  def send(%Socket{encoder: :unset} = socket, data) do
    %Socket{adapter: adapter, adapter_state: adapter_state} = socket
    adapter.send(adapter_state, data)
  end

  def send(%Socket{} = socket, message) do
    %Socket{adapter: adapter, adapter_state: adapter_state, encoder: encoder} = socket
    data = encoder.encode(message, socket)
    adapter.send(adapter_state, data)
  end

  @doc """
  Sets transport options for the socket.
  """
  @spec setopts(Socket.t(), list()) :: :ok | {:error, term()}
  def setopts(%Socket{} = socket, opts) do
    %Socket{adapter: adapter, adapter_state: adapter_state} = socket
    adapter.setopts(adapter_state, opts)
  end

  @doc """
  Closes the socket through its transport.
  """
  @spec close(Socket.t()) :: :ok
  def close(%Socket{} = socket) do
    %Socket{adapter: adapter, adapter_state: adapter_state} = socket
    adapter.close(adapter_state)
  end

  @doc """
  Adds key value pairs to socket assigns.

  A single key value pair may be passed, a keyword list or map
  of assigns may be provided to be merged into existing socket
  assigns.

  ## Examples

      iex> assign(socket, :name, "ElvenGard")
      iex> socket.assigns.name == "ElvenGard"
      true

      iex> assign(socket, name: "ElvenGard", logo: "🌸")
      iex> socket.assigns.name == "ElvenGard"
      true
      iex> socket.assigns.logo == "🌸"
      true
  """
  @spec assign(Socket.t(), atom(), any()) :: Socket.t()
  def assign(%Socket{} = socket, key, value) do
    assign(socket, [{key, value}])
  end

  @spec assign(Socket.t(), map() | keyword()) :: Socket.t()
  def assign(%Socket{} = socket, attrs) when is_map(attrs) or is_list(attrs) do
    %{socket | assigns: Map.merge(socket.assigns, Map.new(attrs))}
  end
end
