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

  """

  alias __MODULE__
  alias ElvenGard.Network.UUID

  defstruct id: nil,
            adapter: nil,
            adapter_state: nil,
            protocol: nil

  @type t :: %Socket{}

  @doc """
  Create a new socket structure.

  This function initializes a new socket with the given `transport_pid`, `transport`,
  and `encoder` module.
  """
  @spec new(module(), any(), module()) :: Socket.t()
  def new(adapter, adapter_state, protocol) do
    %Socket{
      id: UUID.uuid4(),
      adapter: adapter,
      adapter_state: adapter_state,
      protocol: protocol
    }
  end
end
