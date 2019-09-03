defmodule ElvenGard.Protocol do
  @moduledoc """
  Transform a raw packet (packet received by a client) into a packet that can be
  pattern match by a PacketHandler

  /!\\ No side effect: Cannot change or modify the current `ElvenGard.Structures.Client`

  TODO: I don't really know if it's usefull to add `pre` and `post` hooks for `encode` and
  `decode`. I'll probably remove them later.
  """

  alias ElvenGard.Structures.Client

  @doc """
  Define customs aliases for fields types
  """
  @callback aliases() :: [{atom, atom}]

  @doc """
  Prepare the packet to be sent for the encode function

  NOTE: This function is called by `ElvenGard.Structures.Client.send/2`
  """
  @callback pre_encode(data :: term, client :: Client.t()) :: term

  @doc """
  Transforms a term into a packet that can be sent to the client

  You can, for example, apply your cryptographic algorithm.
  """
  @callback encode(data :: term) :: term

  @doc """
  If not already done by the `c:encode/1` function, this function will transform his
  result into a binary.
  """
  @callback post_encode(data :: term, client :: Client.t()) :: binary

  @doc """
  Prepare the raw packet for the decode function
  """
  @callback pre_decode(data :: binary, client :: Client.t()) :: term

  @doc """
  Transform a raw packet to an understandable packet.

  You can, for example, apply your cryptographic algorithm and split your packet.
  """
  @callback decode(data :: term) :: term

  @doc """
  If not already done by the `c:decode/1` function, this function will transform his
  result into a tuple.

  NOTE: This function must return a tuple starting with your packet header followed by
  params or a list of tuple.  
  The result of this function will then be used by `c:ElvenGard.Packet.handle_packet/3`
  """
  @callback post_decode(data :: term, client :: Client.t()) :: {term, map} | list(tuple)

  @doc """
  Use ElvenGard.Protocol behaviour
  """
  defmacro __using__(_) do
    parent = __MODULE__

    quote do
      @behaviour unquote(parent)

      alias ElvenGard.Structures.Client

      @before_compile unquote(parent)

      @doc """
      Successively applies functions `pre_encode`, `encode` and `post_encode`
      """
      @spec complete_encode(term, Client.t()) :: binary
      def complete_encode(data, %Client{} = client) do
        data
        |> pre_encode(client)
        |> encode()
        |> post_encode(client)
      end

      @doc """
      Successively applies functions `pre_decode`, `decode` and `post_decode`

      NOTE: Can return a packet list
      """
      @spec complete_decode(binary, Client.t()) :: tuple | list(tuple)
      def complete_decode(data, %Client{} = client) do
        data
        |> pre_decode(client)
        |> decode()
        |> post_decode(client)
      end

      #
      # Protocol behaviour
      #

      @impl true
      def aliases(), do: []

      @impl true
      def pre_encode(data, _client), do: data

      @impl true
      def post_encode(data, _client), do: data

      @impl true
      def pre_decode(data, _client), do: data

      @impl true
      def post_decode(data, _client), do: data

      defoverridable aliases: 0,
                     pre_encode: 2,
                     post_encode: 2,
                     pre_decode: 2,
                     post_decode: 2,
                     complete_encode: 2,
                     complete_decode: 2
    end
  end

  defmacro __before_compile__(env) do
    unless Module.defines?(env.module, {:encode, 1}) do
      raise """
      function encode/1 required by behaviour #{__MODULE__} is not implemented \
      (in module #{env.module}).

      Example:
        def encode(data) do
          Crypto.encrypt(data)
        end
      """
    end

    unless Module.defines?(env.module, {:decode, 1}) do
      raise """
      function decode/1 required by behaviour #{__MODULE__} is not implemented \
      (in module #{env.module}).

      Example:
        def decode(data) do
          data
          |> Crypto.decrypt()
          |> String.split(" ")
        end
      """
    end
  end
end
