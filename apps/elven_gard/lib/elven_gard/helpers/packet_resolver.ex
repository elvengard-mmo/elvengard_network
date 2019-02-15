defmodule ElvenGard.Helpers.PacketResolver do
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
  @callback resolve(client :: Client.t(), data :: binary) :: handle_return

  @doc """
  Transform a raw packet to an understandable packet.
  You can, for example, apply your cryptographic algorithm and split your packet.
  This function must return a list starting with your packet header followed by params.
  """
  @callback deserialize(data :: term) :: list

  @doc """
  Use ElvenGard.Helpers.PacketResolver behaviour
  """
  defmacro __using__(opts) do
    parent = __MODULE__
    caller = __CALLER__.module
    handler = get_in(opts, [:packet_handler])

    # Check is there is any handler
    case handler do
      nil -> raise "Please, specify a packet_handler for #{caller}"
      _ -> :ok
    end

    quote do
      @behaviour unquote(parent)
      @before_compile unquote(parent)

      alias ElvenGard.Structures.Client

      def resolve(%Client{} = client, data) do
        data
        |> deserialize()
        |> handle_packet(client)
      end

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

      defoverridable resolve: 2
    end
  end

  defmacro __before_compile__(env) do
    unless Module.defines?(env.module, {:deserialize, 1}) do
      raise """
      function deserialize/1 required by behaviour #{__MODULE__} is not implemented \
      (in module #{env.module}).

      Example:
        def deserialize(data) do
          data
          |> Crypto.decrypt()
          |> String.split(" ")
        end
      """
    end
  end
end
