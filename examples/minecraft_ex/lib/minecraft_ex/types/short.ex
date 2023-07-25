defmodule MinecraftEx.Types.Short do
  @moduledoc """
  16-bit integer signed or unsigned
  ===

  Can be signed or unsigned

  # Unsigned Short

  An integer between 0 and 65535

  ## Examples

      iex> MinecraftEx.Types.Short.decode(<<0xFFFF::16>>, sign: :unsigned)
      {65535, ""}


  # Signed Short

  An integer between -32768 and 32767: Signed 16-bit integer, two's complement 

  ## Examples

      iex> MinecraftEx.Types.Short.decode(<<0xFFFF::16>>, sign: :signed)
      {-1, ""}
  """

  use ElvenGard.Network.Type

  @type t :: 0..65535

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, opts) when is_binary(data) do
    sign = opts[:sign] || :unsigned

    case sign do
      :unsigned ->
        <<value::unsigned-16, rest::bitstring>> = data
        {value, rest}

      :signed ->
        <<value::signed-16, rest::bitstring>> = data
        {value, rest}
    end
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(_data, _opts), do: raise("unimplemented for now")
end
