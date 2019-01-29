defmodule ElvenGard.Game.PacketResolver do
  @moduledoc """
  Documentation for ElvenGard.Game.LoginServer.
  """

  alias ElvenGard.Game.Client
  alias ElvenGard.Game.LoginServer

  @type state :: Client.t()

  @doc """
  Just split a packet and call his packet handler
  """
  @callback resolve(client :: Client.t(), data :: binary) ::
              {:cont, state}
              | {:halt, {:ok, term}, state}
              | {:halt, {:error, LoginServer.conn_error()}, state}

  @doc """
  Transform a raw packet to an understandable packet.
  You can, for example, apply your cryptographic algorithm and split your packet.
  This function must return a list starting with your packet header followed by params.
  """
  @callback deserialize(data :: term) :: list

  @doc """
  Use ElvenGard.Game.PacketResolver behaviour.
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

    quote bind_quoted: [parent: parent, caller: caller, handler: handler] do
      @behaviour parent
      @before_compile parent

      alias ElvenGard.Game.Client

      def resolve(%Client{} = client, data) do
        data
        |> deserialize()
        |> unquote(handler).handle_packet(client)
      end

      defp handle_packet(packet, %Client{} = client) do
        unquote(handler).handle_packet(packet, client)
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
