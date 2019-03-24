defmodule ElvenGard.Helpers.PacketEncoder do
  @moduledoc """
  Transform a raw packet (packet received by a client) into a packet that can be
  pattern match by a PacketHandler.

  /!\\ No side effect: Cannot change or modify the current `ElvenGard.Structures.Client`
  """

  alias ElvenGard.Structures.Client

  @doc """
  Prepare the packet to be sent for the encode function
  """
  @callback pre_encode(data :: term, client :: Client.t()) :: term

  @doc """
  Encodes a packet so that it can be sent to a client.
  You can, for example, apply your cryptographic algorithm.
  """
  @callback encode(data :: term) :: term

  @doc """
  If not already done by the `encode` function, this function will transform his
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
  If not already done by the `decode` function, this function will transform his
  result into a tuple.
  This function must return a tuple starting with your packet header followed by params.
  """
  @callback post_decode(data :: term, client :: Client.t()) :: {term, map} | list(tuple)

  @doc """
  Use ElvenGard.Helpers.PacketEncoder behaviour
  """
  defmacro __using__(opts) do
    parent = __MODULE__
    mode = opts[:mode]
    model = opts[:model]

    quote do
      @behaviour unquote(parent)
      @before_compile unquote(parent)

      alias ElvenGard.Structures.Client

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
      Can return a packet list
      """
      @spec complete_decode(binary, Client.t()) :: tuple | list(tuple)

      if unquote(mode) == :binary,
        do: unquote(parent.def_binary_decode(model)),
        else: unquote(parent.def_normal_decode())

      #
      # PacketEncoder behaviour
      #

      @impl true
      def pre_encode(data, _client), do: data

      @impl true
      def post_encode(data, _client), do: data

      @impl true
      def pre_decode(data, _client), do: data

      @impl true
      def post_decode(data, _client), do: data

      defoverridable pre_encode: 2
      defoverridable post_encode: 2
      defoverridable pre_decode: 2
      defoverridable post_decode: 2
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

  #
  # Private functions
  #

  @doc false
  def def_normal_decode() do
    quote do
      def complete_decode(data, %Client{} = client) do
        data
        |> pre_decode(client)
        |> decode()
        |> post_decode(client)
      end
    end
  end

  @doc false
  def def_binary_decode(model) do
    quote do
      ## Principal decoder
      def complete_decode(data, %Client{} = client) do
        data
        |> pre_decode(client)
        |> decode()
        |> post_decode(client)
        |> binary_decode()
      end

      ## Define sub decoders
      Enum.each(unquote(model).elven_get_packet_documentation(), fn packet ->
        name = packet.name
        fields = Macro.escape(packet.fields)

        contents =
          quote do
            defp binary_decode({unquote(name), params}) do
              res = do_binary_decode(params, unquote(fields), %{})
              {unquote(name), res}
            end
          end

        Module.eval_quoted(__MODULE__, contents)
      end)

      @doc false
      @spec do_binary_decode(bitstring, list, map) :: map
      defp do_binary_decode(_bin, [], params), do: params

      defp do_binary_decode(bin, [field | tail_fields], params) do
        %{
          name: name,
          type: type,
          opts: opts
        } = field

        {val, rest} = type.decode(bin, opts)
        do_binary_decode(rest, tail_fields, Map.put(params, name, val))
      end
    end
  end
end
