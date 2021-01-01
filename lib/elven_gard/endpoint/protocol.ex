defmodule ElvenGard.Endpoint.Protocol do
  @moduledoc """
  TODO: Documentation
  """

  alias ElvenGard.Socket

  @callback handle_init(socket :: Socket.t()) ::
              {:ok, new_socket}
              | {:ok, new_socket, timeout() | :hibernate | {:continue, term()}}
              | {:stop, reason :: term(), new_socket}
            when new_socket: Socket.t()

  @callback handle_message(message :: binary(), socket :: Socket.t()) ::
              :ignore
              | {:ignore, new_socket}
              | {:ok, new_socket}
              | {:stop, reason :: term(), new_socket}
            when new_socket: Socket.t()

  @callback handle_halt(reason :: term(), socket :: Socket.t()) ::
              {:ok, new_socket}
              | {:ok, stop_reason :: term(), new_socket}
            when new_socket: term()

  @optional_callbacks handle_init: 1,
                      handle_message: 2,
                      handle_halt: 2

  ## Public API

  @doc false
  defmacro __using__(opts) do
    quote do
      use GenServer

      @behaviour unquote(__MODULE__)
      @behaviour :ranch_protocol

      unquote(defs(opts))
      unquote(message_callbacks())
      unquote(halt_callbacks())
      unquote(default_callbacks())
    end
  end

  ## Private functions

  defp defs(using_opts) do
    quote location: :keep do
      @doc false
      def start_link(ref, transport, opts) do
        pid = :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])
        {:ok, pid}
      end

      @impl true
      def init({ref, transport, opts}) do
        {:ok, transport_pid} = :ranch.handshake(ref)

        serializer_mod = get_serializer(opts, unquote(using_opts))
        socket = Socket.new(transport_pid, transport, serializer_mod, self())

        case handle_init(socket) do
          {:ok, new_socket} -> do_enter_loop(new_socket)
          {:ok, new_socket, timeout} -> do_enter_loop(new_socket, timeout)
          {:stop, reason, new_socket} -> {:stop, reason, new_socket}
          _ -> raise "handle_init/1 must return `{:ok, socket}` or `{:ok, socket, timeout}`"
        end
      end

      ## Helpers

      defp do_enter_loop(%Socket{} = socket, timeout \\ :infinity) do
        %Socket{transport: transport, transport_pid: transport_pid} = socket
        transport.setopts(transport_pid, active: :once)
        :gen_server.enter_loop(__MODULE__, [], socket, timeout)
      end

      defp get_serializer(config_opts, using_opts) do
        case {config_opts[:serializer], using_opts[:serializer]} do
          {nil, nil} ->
            IO.warn("""
              no serializer found for `#{inspect(__MODULE__)}`.
              We will use `ElvenGard.Socket.DummySerializer` for now.
            """)

            ElvenGard.Socket.DummySerializer

          {mod, nil} ->
            mod

          {nil, mod} ->
            mod

          {mod1, mod2} ->
            if mod1 != mod2 do
              IO.warn("""
                Duplicate `:serializer` key found for #{inspect(__MODULE__)}.
                
                The given values were found:
                  - from protocol configuration (`config.exs`): #{inspect(mod1)}
                  - from using options: #{inspect(mod2)}
                  
                The value from using options will be ignored.
              """)
            end

            mod1
        end
      end
    end
  end

  defp message_callbacks() do
    quote location: :keep do
      @impl true
      def handle_info({:tcp, transport_pid, data}, %Socket{} = socket) do
        %Socket{transport: transport} = socket

        result =
          case handle_message(data, socket) do
            :ignore -> {:noreply, socket}
            {:ignore, new_socket} -> {:noreply, new_socket}
            {:ok, new_socket} -> dispatch_message(data, new_socket)
            {:stop, reason, new_socket} -> {:stop, reason, new_socket}
            term -> raise "invalid return value for handle_message/2 (got: #{inspect(term)})"
          end

        transport.setopts(transport_pid, active: :once)
        result
      end

      ## Helpers

      defp dispatch_message(data, %Socket{} = socket) do
        raise "TODO: implement"
      end
    end
  end

  defp halt_callbacks() do
    quote location: :keep do
      @impl true
      def handle_info({:tcp_closed, transport_pid}, %Socket{} = socket) do
        %Socket{transport: transport} = socket
        transport.close(transport_pid)
        do_handle_halt(:tcp_closed, socket)
      end

      @impl true
      def handle_info(:timeout, %Socket{} = socket) do
        do_handle_halt(:timeout, socket)
      end

      ## Helpers

      defp do_handle_halt(reason, %Socket{} = socket) do
        case handle_halt(reason, socket) do
          {:ok, new_socket} -> {:stop, :normal, new_socket}
          {:ok, stop_reason, new_socket} -> {:stop, stop_reason, new_socket}
          _ -> raise "handle_halt/2 must return `{:ok, socket}` or `{:ok, stop_reason, socket}`"
        end
      end
    end
  end

  defp default_callbacks() do
    quote location: :keep do
      @impl true
      def handle_init(socket), do: {:ok, socket}

      @impl true
      def handle_message(_message, socket), do: {:ok, socket}

      @impl true
      def handle_halt(_reason, socket), do: {:ok, socket}

      defoverridable handle_init: 1,
                     handle_message: 2,
                     handle_halt: 2
    end
  end
end
