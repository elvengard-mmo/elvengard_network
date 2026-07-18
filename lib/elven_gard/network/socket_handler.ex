defmodule ElvenGard.Network.SocketHandler do
  @moduledoc """
  Defines transport-independent callbacks for handling a socket connection.

  A socket handler initializes connection state, decides how incoming data is
  processed, and performs cleanup when the connection halts. The endpoint
  runtime is responsible for translating these results to its networking
  implementation.
  """

  alias ElvenGard.Network.Socket

  @type stop_reason :: any()

  @type init_action ::
          timeout()
          | :hibernate
          | {:continue, any()}

  @type init_result ::
          {:ok, Socket.t()}
          | {:ok, Socket.t(), init_action()}
          | {:stop, stop_reason(), Socket.t()}

  @type message_result ::
          :ignore
          | {:ignore, Socket.t()}
          | {:ok, Socket.t()}
          | {:stop, stop_reason(), Socket.t()}

  @type halt_result ::
          {:ok, Socket.t()}
          | {:ok, stop_reason(), Socket.t()}

  @doc """
  Initializes a newly connected socket before the endpoint starts receiving data.
  """
  @callback handle_init(socket :: Socket.t()) :: init_result()

  @doc """
  Handles raw data received from the connection.

  Returning `:ignore` or `{:ignore, socket}` skips packet decoding. Returning
  `{:ok, socket}` passes the data to the configured network codec and packet
  handler.
  """
  @callback handle_message(message :: binary(), socket :: Socket.t()) :: message_result()

  @doc """
  Performs cleanup after the endpoint halts the connection.
  """
  @callback handle_halt(reason :: stop_reason(), socket :: Socket.t()) :: halt_result()

  @optional_callbacks handle_init: 1,
                      handle_message: 2,
                      handle_halt: 2

  @doc false
  @spec __using__(Macro.t()) :: Macro.t()
  defmacro __using__(_opts) do
    quote location: :keep do
      @behaviour ElvenGard.Network.SocketHandler

      @impl ElvenGard.Network.SocketHandler
      def handle_init(socket), do: {:ok, socket}

      @impl ElvenGard.Network.SocketHandler
      def handle_message(_message, socket), do: {:ok, socket}

      @impl ElvenGard.Network.SocketHandler
      def handle_halt(_reason, socket), do: {:ok, socket}

      defoverridable handle_init: 1,
                     handle_message: 2,
                     handle_halt: 2
    end
  end
end
