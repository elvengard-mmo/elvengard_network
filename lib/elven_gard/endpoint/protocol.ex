defmodule ElvenGard.Endpoint.Protocol do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Socket

  @callback handle_init(state) ::
              {:ok, state}
              | {:ok, state, timeout() | :hibernate | {:continue, term()}}
            when state: any()

  @optional_callbacks handle_init: 1

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      use GenServer

      @behaviour unquote(__MODULE__)

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

        case handle_init(state) do
          {:ok, state} -> :gen_server.enter_loop(__MODULE__, [], state)
          {:ok, state, timeout} -> :gen_server.enter_loop(__MODULE__, [], state, timeout)
          _ -> raise "handle_init/2 must return a `{:ok, state}` or `{:ok, state, timeout}`"
        end
      end
    end
  end

  defp defs() do
    quote location: :keep do
      @impl true
      def handle_init(state), do: {:ok, state}

      defoverridable handle_init: 1
    end
  end
end
