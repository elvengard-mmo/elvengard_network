defmodule ElvenGard.Socket.Serializer do
  @moduledoc """
  A behaviour that serializes incoming and outgoing socket messages.
  """

  @doc """
  Define customs aliases for fields types
  """
  @callback aliases() :: [keyword(module()), ...]

  @doc """
  Transforms a term into a packet that can be sent to the Socket

  You can, for example, apply your cryptographic algorithm 
  (encryption).  
  NOTE: This function is called by `ElvenGard.Socket.send/2`
  """
  @callback encode!(data :: any(), socket :: Socket.t()) :: iodata()

  @doc """
  Transform a raw packet to an understandable packet

  You can, for example, apply your cryptographic algorithm 
  (decryption) and split your packet.

  NOTE: This function must returns a tuple starting with a 
  packet header followed by params or a list of this kind 
  of tuple.  
  The result of this function will then be passed in 
  `c:ElvenGard.PacketHandler.handle_packet/3`
  """
  @callback decode!(data :: bitstring(), socket :: Socket.t()) ::
              {any(), list()} | [{any(), list()}, ...]

  ## Define some helpers

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)

      @doc """
      Returns the module associated to an alias or the given value if no alias has been found
      """
      @spec alias_for(atom() | module()) :: atom() | module()
      def alias_for(value), do: Keyword.get(aliases(), value, value)

      @doc """
      Same as `alias_for/1` but raise an Exception if no alias has been found
      """
      @spec alias_for!(atom()) :: module()
      def alias_for!(value), do: Keyword.fetch!(aliases(), value)
    end
  end
end
