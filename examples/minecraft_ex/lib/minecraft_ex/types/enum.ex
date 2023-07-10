defmodule MinecraftEx.Types.Enum do
  @moduledoc """
  TODO

  ===

  TODO
  """

  @behaviour ElvenGard.Network.Type

  @type t :: :atom

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, opts) when is_binary(data) do
    # Enum, from: VarInt, values: [status: 1, login: 2]
    {from, opts} = Keyword.pop!(opts, :from)
    {enumerators, opts} = Keyword.pop!(opts, :values)

    {value, rest} = apply(from, :decode, [data, opts])
    {key, _v} = Enum.find(enumerators, &(elem(&1, 1) == value))

    {key, rest}
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(_data, _opts), do: raise("unimplemented for now")
end
