defmodule ElvenGard.Network.Endpoint.Protocol do
  @moduledoc ~S"""
  Wrapper on top of Ranch's [protocols](https://ninenines.eu/docs/en/ranch/2.1/guide/protocols/).

  This module defines a protocol behavior to handle incoming connections in the
  ElvenGard.Network library. It provides callbacks for initializing, handling
  incoming messages, and handling connection termination.

  This protocol behavior serves as a wrapper around Ranch protocols, providing
  a structured way to implement connection handling within ElvenGard.Network.

  For detailed information on implementing and using network protocols
  with ElvenGard.Network, please refer to the [Endpoint Protocol guide]
  (<ENDPOINT_PROTOCOL_DOCS_URL>).
  """

  alias ElvenGard.Network.Socket

  @doc """
  Callback called just before entering the GenServer loop.

  This callback is invoked when a new connection is established and before the
  GenServer loop starts processing messages.

  For the return values, see `c:GenServer.init/1`
  """
  @callback handle_init(socket :: Socket.t()) ::
              {:ok, new_socket}
              | {:ok, new_socket, timeout | :hibernate | {:continue, continue_arg}}
              | {:stop, reason :: term, new_socket}
            when new_socket: Socket.t(), continue_arg: term

  @doc """
  Callback called just after receiving a message.

  For the return values, see `c:GenServer.handle_info/2`
  """
  @callback handle_message(message :: binary, socket :: Socket.t()) ::
              :ignore
              | {:ignore, new_socket}
              | {:ok, new_socket}
              | {:stop, reason :: term, new_socket}
            when new_socket: Socket.t()

  @doc """
  Callback called after the socket connection is closed and before the GenServer
  shutdown.

  For the return values, see `c:GenServer.handle_info/2`
  """
  @callback handle_halt(reason :: term, socket :: Socket.t()) ::
              {:ok, new_socket}
              | {:ok, stop_reason :: term, new_socket}
            when new_socket: term

  @optional_callbacks handle_init: 1,
                      handle_message: 2,
                      handle_halt: 2

  ## Public API

  @doc false
  defmacro __using__(_opts) do
    quote do
      use GenServer

      @behaviour unquote(__MODULE__)
      @behaviour :ranch_protocol

      unquote(defs())
      unquote(message_callbacks())
      unquote(halt_callbacks())
      unquote(default_callbacks())
    end
  end

  ## Private functions

  defp defs() do
    quote location: :keep do
      @impl :ranch_protocol
      def start_link(ref, transport, opts) do
        {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
      end

      @impl GenServer
      def init({ref, transport, opts}) do
        {:ok, transport_pid} = :ranch.handshake(ref)
        socket = Socket.new(transport_pid, transport, codec())

        init_error =
          "handle_init/1 must return `{:ok, socket}`, `{:ok, socket, timeout}` " <>
            "or `{:stop, reason, new_socket}`"

        case handle_init(socket) do
          {:ok, new_socket} -> do_enter_loop(new_socket)
          {:ok, new_socket, timeout} -> do_enter_loop(new_socket, timeout)
          {:stop, reason, new_socket} -> {:stop, reason, new_socket}
          _ -> raise init_error
        end
      end

      ## Helpers

      defp do_enter_loop(%Socket{} = socket, timeout \\ :infinity) do
        %Socket{transport: transport, transport_pid: transport_pid} = socket
        transport.setopts(transport_pid, active: :once)
        :gen_server.enter_loop(__MODULE__, [], socket, timeout)
      end
    end
  end

  # credo:disable-for-next-line
  defp message_callbacks() do
    quote location: :keep do
      @impl true
      def handle_info({:tcp, transport_pid, data}, %Socket{} = socket) do
        %Socket{transport: transport, remaining: remaining} = socket
        full_data = <<remaining::bitstring, data::bitstring>>

        result =
          case handle_message(full_data, socket) do
            :ignore -> {:noreply, socket}
            {:ignore, new_socket} -> {:noreply, new_socket}
            {:ok, new_socket} -> packet_loop(full_data, new_socket)
            {:stop, reason, new_socket} -> {:stop, reason, new_socket}
            term -> raise "invalid return value for handle_message/2 (got: #{inspect(term)})"
          end

        transport.setopts(transport_pid, active: :once)
        result
      end

      ## Helpers

      @app Mix.Project.get().project[:app]
      defp env_config(), do: Application.fetch_env!(@app, __MODULE__)
      defp codec(), do: env_config()[:network_codec]
      defp handlers(), do: env_config()[:packet_handler]

      defp packet_loop(<<>>, socket), do: {:noreply, socket}

      defp packet_loop(data, socket) do
        with {:next, {raw, rest}} when not is_nil(raw) <- {:next, codec().next(data, socket)},
             struct <- codec().decode(raw, socket),
             {:handle, {:cont, new_socket}} <- {:handle, handlers().handle_packet(struct, socket)} do
          packet_loop(rest, new_socket)
        else
          {:next, {nil, rest}} -> {:noreply, %Socket{socket | remaining: rest}}
          {:handle, {:halt, new_socket}} -> do_handle_halt(:normal, socket)
          {:handle, {:halt, reason, new_socket}} -> do_handle_halt(reason, socket)
        end
      end
    end
  end

  defp halt_callbacks() do
    quote location: :keep do
      @impl true
      def handle_info({:tcp_closed, transport_pid}, %Socket{} = socket) do
        do_handle_halt(:tcp_closed, socket)
      end

      @impl true
      def handle_info(:timeout, %Socket{} = socket) do
        do_handle_halt(:timeout, socket)
      end

      ## Helpers

      defp do_handle_halt(reason, socket) do
        %Socket{transport: transport, transport_pid: transport_pid} = socket
        transport.close(transport_pid)

        case handle_halt(reason, socket) do
          {:ok, new_socket} -> {:stop, :normal, new_socket}
          {:ok, stop_reason, new_socket} -> {:stop, stop_reason, new_socket}
          _ -> raise "handle_halt/2 must return `{:ok, socket}` or `{:ok, stop_reason, socket}`"
        end
      end
    end
  end

  defp default_callbacks() do
    quote do
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
