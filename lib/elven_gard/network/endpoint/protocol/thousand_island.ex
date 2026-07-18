if Code.ensure_loaded?(ThousandIsland.Handler) do
  defmodule ElvenGard.Network.Endpoint.Protocol.ThousandIsland do
    @moduledoc false

    use ThousandIsland.Handler

    alias ElvenGard.Network.Endpoint.MessageProcessor
    alias ElvenGard.Network.Socket
    alias ElvenGard.Network.Socket.Adapters.ThousandIsland, as: ThousandIslandAdapter

    defmodule State do
      @moduledoc false

      alias ElvenGard.Network.Socket
      alias ElvenGard.Network.SocketHandler

      @enforce_keys [:socket, :socket_handler, :packet_handler]
      defstruct [:socket, :socket_handler, :packet_handler, halt_reason: :closed]

      @type t :: %__MODULE__{
              socket: Socket.t(),
              socket_handler: module(),
              packet_handler: module(),
              halt_reason: SocketHandler.stop_reason()
            }
    end

    ## ThousandIsland.Handler callbacks

    @impl ThousandIsland.Handler
    def handle_connection(transport_socket, options) do
      socket_handler = Keyword.fetch!(options, :socket_handler)
      otp_app = Keyword.fetch!(options, :otp_app)
      handler_config = Application.fetch_env!(otp_app, socket_handler)
      codec = Keyword.fetch!(handler_config, :network_codec)
      packet_handler = Keyword.fetch!(handler_config, :packet_handler)

      socket = Socket.new(ThousandIslandAdapter, [socket: transport_socket], codec)

      state = %State{
        socket: socket,
        socket_handler: socket_handler,
        packet_handler: packet_handler
      }

      case socket_handler.handle_init(socket) do
        {:ok, %Socket{} = new_socket} ->
          {:continue, %State{state | socket: new_socket}}

        {:ok, %Socket{} = new_socket, timeout} ->
          {:continue, %State{state | socket: new_socket}, timeout}

        {:stop, reason, %Socket{} = new_socket} ->
          close(reason, new_socket, state)

        _ ->
          raise "handle_init/1 must return `{:ok, socket}`, `{:ok, socket, timeout}` " <>
                  "or `{:stop, reason, new_socket}`"
      end
    end

    @impl ThousandIsland.Handler
    def handle_data(data, transport_socket, %State{} = state) do
      %State{
        socket: %Socket{} = socket,
        socket_handler: socket_handler,
        packet_handler: packet_handler
      } = state

      socket = %Socket{socket | adapter_state: transport_socket}

      case MessageProcessor.process(data, socket, socket_handler, packet_handler) do
        {:cont, new_socket} -> {:continue, %State{state | socket: new_socket}}
        {:halt, reason, new_socket} -> close(reason, new_socket, state)
      end
    end

    @impl ThousandIsland.Handler
    def handle_close(transport_socket, %State{} = state) do
      %State{socket: %Socket{} = socket, halt_reason: reason} = state
      socket = %Socket{socket | adapter_state: transport_socket}
      handle_halt(reason, socket, state)
    end

    @impl ThousandIsland.Handler
    def handle_error(reason, transport_socket, %State{} = state) do
      %State{socket: %Socket{} = socket} = state
      socket = %Socket{socket | adapter_state: transport_socket}
      handle_halt({:error, reason}, socket, state)
    end

    @impl ThousandIsland.Handler
    def handle_shutdown(transport_socket, %State{} = state) do
      %State{socket: %Socket{} = socket} = state
      socket = %Socket{socket | adapter_state: transport_socket}
      handle_halt(:closed, socket, state)
    end

    @impl ThousandIsland.Handler
    def handle_timeout(transport_socket, %State{} = state) do
      %State{socket: %Socket{} = socket} = state
      socket = %Socket{socket | adapter_state: transport_socket}
      handle_halt(:timeout, socket, state)
    end

    ## Private function

    defp close(reason, socket, %State{} = state) do
      {:close, %State{state | socket: socket, halt_reason: reason}}
    end

    defp handle_halt(reason, socket, %State{} = state) do
      %State{socket_handler: socket_handler} = state

      case socket_handler.handle_halt(reason, socket) do
        {:ok, %Socket{}} ->
          :ok

        {:ok, _stop_reason, %Socket{}} ->
          :ok

        _ ->
          raise "handle_halt/2 must return `{:ok, socket}` or `{:ok, stop_reason, socket}`"
      end
    end
  end
end
