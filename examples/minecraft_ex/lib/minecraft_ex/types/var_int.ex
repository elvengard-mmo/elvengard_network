defmodule MinecraftEx.Types.VarInt do
  @moduledoc """
  An integer between -2147483648 and 2147483647 

  ===

  Variable-length format such that smaller numbers use fewer bytes.

  These are very similar to Protocol Buffer Varints: the 7 least 
  significant bits are used to encode the value and the most significant 
  bit indicates whether there's another byte after it for the next part 
  of the number. The least significant group is written first, followed 
  by each of the more significant groups; thus, VarInts are effectively 
  little endian (however, groups are 7 bits, not 8).

  VarInts are never longer than 5 bytes.
  """

  import Bitwise, only: [{:&&&, 2}, {:|||, 2}, {:<<<, 2}, {:>>>, 2}]

  use ElvenGard.Network.Type

  @type t :: -2_147_483_648..2_147_483_647

  ## Behaviour impls

  @impl true
  @spec decode(bitstring, keyword) :: {t(), bitstring}
  def decode(data, _opts) when is_binary(data) do
    do_decode(data)
  end

  @impl true
  @spec encode(t(), keyword) :: bitstring
  def encode(data, _opts) when is_integer(data) do
    <<udata::unsigned-32>> = <<data::signed-32>>
    do_encode(udata)
  end

  ## Helpers

  defp do_decode(data, offset \\ 0, result \\ 0)

  defp do_decode(_data, offset, _result) when offset >= 5 do
    raise "VarInt is too big"
  end

  defp do_decode(<<cont::1, segment::7, rest::bitstring>>, offset, result) do
    new_result = result ||| segment <<< (offset * 7)

    case cont do
      0 -> {new_result, rest}
      1 -> do_decode(rest, offset + 1, new_result)
    end
  end

  defp do_encode(value, result \\ <<>>) do
    case value <= 0x7F do
      true -> <<result::binary, 0::1, value::7>>
      false -> do_encode(value >>> 7, <<result::binary, 1::1, value &&& 0x7F::7>>)
    end
  end
end
