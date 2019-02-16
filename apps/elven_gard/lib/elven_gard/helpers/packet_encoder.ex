defmodule ElvenGard.Helpers.PacketEncoder do
  @moduledoc """
  TODO: Documentation for ElvenGard.Game.LoginServer
  """

  alias ElvenGard.Game.LoginServer
  alias ElvenGard.Structures.Client

  @type state :: Client.t()
  @type handle_return ::
          {:cont, state}
          | {:halt, {:ok, term}, state}
          | {:halt, {:error, LoginServer.conn_error()}, state}

  @doc """
  Just split a packet and call his packet handler
  """
  @callback decode_and_handle(client :: Client.t(), data :: binary) :: handle_return

  @doc """
  Transform a raw packet to an understandable packet.
  You can, for example, apply your cryptographic algorithm and split your packet.
  This function must return a list starting with your packet header followed by params.
  """
  @callback decode(data :: term) :: list

  @doc """
  Encodes a packet so that it can be sent to a client.
  You can, for example, apply your cryptographic algorithm.
  """
  @callback encode(data :: term) :: binary

  @doc """
  Use ElvenGard.Helpers.PacketEncoder behaviour
  """
  defmacro __using__(opts) do
    parent = __MODULE__
    caller = __CALLER__.module
    handler = get_in(opts, [:packet_handler])

    # Check is there is any handler
    unless handler do
      raise "Please, specify a packet_handler for #{caller}"
    end

    quote do
      @behaviour unquote(parent)
      @before_compile unquote(parent)

      alias ElvenGard.Structures.Client

      @impl true
      def decode_and_handle(%Client{} = client, data) do
        data
        |> decode()
        |> handle_packet(client)
      end

      #
      # Some utils functions
      #

      @spec handle_packet(list(term), Client.t()) :: unquote(parent).handle_return
      defp handle_packet(packet, %Client{} = client) do
        unquote(handler).handle_packet(packet, client)
      end

      @spec handle_packets(list(list(term)), Client.t()) :: unquote(parent).handle_return
      defp handle_packets(packet_list, %Client{} = client) do
        Enum.reduce_while(packet_list, {:cont, client}, fn packet, {_, client} ->
          res = handle_packet(packet, client)
          {elem(res, 0), res}
        end)
      end

      defoverridable decode_and_handle: 2
    end
  end

  defmacro __before_compile__(env) do
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
  end
end
