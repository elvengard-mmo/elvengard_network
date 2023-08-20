defmodule MinecraftEx.Types.Enum do
  @moduledoc """
  A specific value from a given list

  ===

  The list of possible values and how each is encoded as an X must be known
  from the context. An invalid value sent by either side will usually result
  in the client being disconnected with an error or even crashing.
  """

  use ElvenGard.Network.Type

  @type t :: :atom

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, opts) when is_binary(data) do
    # Enum, from: VarInt, values: [status: 1, login: 2]
    {from, opts} = Keyword.pop!(opts, :from)
    {enumerators, opts} = Keyword.pop!(opts, :values)

    {value, rest} = from.decode(data, opts)
    {key, _v} = Enum.find(enumerators, &(elem(&1, 1) == value))

    {key, rest}
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(_data, _opts), do: raise("unimplemented for now")
end
