defmodule ElvenGard.Endpoint.Protocol do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Socket

  @callback handle_connection(state :: term()) ::
              {:ok, new_state}
              | {:ok, new_state, timeout() | :hibernate | {:continue, term()}}
            when new_state: term()

  @callback handle_halt(reason :: term(), state :: term()) ::
              {:ok, new_state}
              | {stop_reason :: term(), new_state}
            when new_state: term()

  @optional_callbacks handle_connection: 1,
                      handle_halt: 2

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      use GenServer

      @behaviour unquote(__MODULE__)
      @behaviour :ranch_protocol

      unquote(protocol())
      unquote(defs())
    end
  end

  ## Private functions

  defp protocol() do
    quote location: :keep do
      @doc false
      def start_link(ref, transport, opts) do
        pid = :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])
        {:ok, pid}
      end

      @impl true
      def init({ref, transport, opts}) do
        {:ok, transport_pid} = :ranch.handshake(ref)
        socket = Socket.new(transport_pid, transport, nil, self())
        state = %{socket: socket, transport: transport, transport_pid: transport_pid}

        case handle_connection(state) do
          {:ok, state} -> :gen_server.enter_loop(__MODULE__, [], state)
          {:ok, state, timeout} -> :gen_server.enter_loop(__MODULE__, [], state, timeout)
          _ -> raise "handle_connection/1 must return `{:ok, state}` or `{:ok, state, timeout}`"
        end
      end

      @impl true
      def handle_info({:tcp_closed, transport_pid} = reason, %{transport: transport} = state) do
        callback_result = handle_halt(reason, state)
        transport.close(transport_pid)

        case callback_result do
          {:ok, new_state} -> {:stop, :normal, new_state}
          {stop_reason, new_state} -> {:stop, stop_reason, new_state}
          _ -> raise "handle_halt/2 must return `{:ok, new_state}` or `{stop_reason, new_state}`"
        end
      end
    end
  end

  defp defs() do
    quote location: :keep do
      @impl true
      def handle_connection(state), do: {:ok, state}

      @impl true
      def handle_halt(_reason, state), do: {:ok, state}

      defoverridable handle_connection: 1,
                     handle_halt: 2
    end
  end
end
