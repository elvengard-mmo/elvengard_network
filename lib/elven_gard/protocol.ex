defmodule ElvenGard.Protocol do
  @moduledoc ~S"""
  Transform a raw packet (packet received by a client) into a packet that can be
  pattern match by a PacketHandler

  /!\\ No side effect: Cannot change or modify the current `ElvenGard.Socket`
  """

  alias ElvenGard.Socket

  @typedoc "Represents a packet header"
  @type packet_header :: term

  @doc """
  Define customs aliases for fields types
  """
  @callback aliases() :: [{atom, atom}, ...]

  @doc """
  Transforms a term into a packet that can be sent to the client

  You can, for example, apply your cryptographic algorithm (encryption).

  NOTE: This function is called by `ElvenGard.Socket.send/2`
  """
  @callback encode(data :: term, socket :: Socket.t()) :: binary

  @doc """
  Transform a raw packet to an understandable packet

  You can, for example, apply your cryptographic algorithm (decryption) and split your packet.

  NOTE: This function must return a tuple starting with your packet header followed by
  params or a list of tuple.  
  The result of this function will then be used by `c:ElvenGard.PacketHandler.handle_packet/3`
  """
  @callback decode(data :: binary, socket :: Socket.t()) ::
              {:error, term} | {packet_header, map} | [{packet_header, map}, ...]

  @doc """
  Use ElvenGard.Protocol behaviour
  """
  defmacro __using__(_) do
    parent = __MODULE__

    quote do
      @behaviour unquote(parent)

      alias ElvenGard.Socket

      @after_compile unquote(parent)

      #
      # Protocol behaviour
      #

      @impl true
      def aliases(), do: []

      defoverridable aliases: 0
    end
  end

  defmacro __after_compile__(env, _bytecode) do
    unless Kernel.function_exported?(env.module, :encode, 2) do
      raise """
      function encode/2 required by behaviour #{inspect(__MODULE__)} is not implemented \
      (in module #{inspect(env.module)}).

      Example:
        @impl #{inspect(__MODULE__)}
        def encode(data, _socket) do
          Crypto.encrypt(data)
        end
      """
    end

    unless Kernel.function_exported?(env.module, :decode, 2) do
      raise """
      function decode/2 required by behaviour #{inspect(__MODULE__)} is not implemented \
      (in module #{env.module}).

      Example:
        @impl #{inspect(__MODULE__)}
        def decode(data, _socket) do
          res =
            data
            |> String.trim()
            |> String.split(" ", parts: 2)

          case res do
            [header] -> {header, %{}}
            [header, params] -> {header, %{params: params}}
          end
        end
      """
    end
  end
end
