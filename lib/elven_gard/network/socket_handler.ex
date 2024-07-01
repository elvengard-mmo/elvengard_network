defmodule ElvenGard.Network.SocketHandler do
  @moduledoc ~S"""
  Wrapper on top of [Ranch protocols](https://ninenines.eu/docs/en/ranch/2.1/guide/protocols/).

  This module defines a protocol behavior to handle incoming connections in the
  ElvenGard.Network library. It provides callbacks for initializing, handling
  incoming messages, and handling connection termination.

  This protocol behavior serves as a wrapper around Ranch protocols, providing
  a structured way to implement connection handling within ElvenGard.Network.

  For detailed information on implementing and using network protocols
  with ElvenGard.Network, please refer to the
  [Endpoint Protocol guide](https://hexdocs.pm/elvengard_network/protocol.html).
  """

  alias ElvenGard.Network.Socket

  @doc """
  Callback called just before entering the GenServer loop.

  This callback is invoked when a new connection is established and before the
  GenServer loop starts processing messages.

  For the return values, see `c:GenServer.init/1`
  """
  @callback handle_connection(socket) ::
              {:ok, socket}
              | {:ok, socket, timeout() | :hibernate | {:continue, continue_arg}}
              | {:stop, reason, socket}
            when socket: Socket.t(), continue_arg: any(), reason: :normal | any()

  @doc """
  Callback called just after receiving a message.

  This callback is invoked whenever a message is received on the connection. It should
  return one of the following:

    - `{:ok, socket}`: classic loop - decode the packet and process it
    - `{:skip, socket}`: the message received will not be decoded or processed by the protocol.
      It will just be skip.
    - `{:stop, reason, socket}`: stop the GenServer/Protocol and disconnect the client

  """
  @callback handle_data(data :: binary(), socket) ::
              {:ok, socket}
              | {:skip, socket}
              | {:stop, reason, socket}
            when socket: Socket.t(), reason: :normal | any()

  @doc """
  This callback is called when the underlying socket is closed by the remote end;
  it should perform any cleanup required as it is the last callback called
  before the process backing this connection is terminated. The underlying
  socket has already been closed by the time this callback is called.

  The return value is ignored.
  """
  @callback handle_close(socket :: Socket.t()) :: any()

  @doc """
  This callback is called when the underlying socket encounters an error;
  it should perform any cleanup required as it is the last callback
  called before the process backing this connection is terminated.
  The underlying socket has already been closed by the time this
  callback is called.

  The return value is ignored.
  """
  @callback handle_error(reason :: any(), socket :: Socket.t()) :: any()

  @doc """
  This callback is called when the server process itself is being shut down;
  it should perform any cleanup required as it is the last callback called
  before the process backing this connection is terminated. The underlying
  socket has NOT been closed by the time this callback is called.

  The return value is ignored.
  """
  @callback handle_shutdown(socket :: Socket.t()) :: any()

  @doc """
  This callback is called when a handler process has gone more than timeout
  ms without receiving either remote data or a local message.

  The return value is ignored.
  """
  @callback handle_timeout(socket :: Socket.t()) :: any()

  @optional_callbacks handle_connection: 1,
                      handle_data: 2,
                      handle_close: 1,
                      handle_error: 2,
                      handle_shutdown: 1,
                      handle_timeout: 1

  @spec __using__(any()) :: Macro.t()
  defmacro __using__(_opts) do
    quote location: :keep do
      @behaviour ElvenGard.Network.SocketHandler

      @impl ElvenGard.Network.SocketHandler
      def handle_connection(socket), do: {:ok, socket}

      @impl ElvenGard.Network.SocketHandler
      def handle_data(_data, socket), do: {:skip, socket}

      @impl ElvenGard.Network.SocketHandler
      def handle_close(socket), do: :ok

      @impl ElvenGard.Network.SocketHandler
      def handle_error(_reason, _socket), do: :ok

      @impl ElvenGard.Network.SocketHandler
      def handle_shutdown(_socket), do: :ok

      @impl ElvenGard.Network.SocketHandler
      def handle_timeout(_socket), do: :ok

      defoverridable handle_connection: 1,
                     handle_data: 2,
                     handle_close: 1,
                     handle_error: 2,
                     handle_shutdown: 1,
                     handle_timeout: 1
    end
  end
end
