defmodule WorldServer.Crypto do
  @moduledoc """
  Cryptography for a Nostale login server.
  """

  ###
  ### THIS MODULE NEED REFACTORING !
  ###

  use Bitwise, only_operators: true

  @table [" ", "-", ".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "n"]

  @doc """
  Encrypt a world packet.

  ## Examples

  """
  @spec encrypt(String.t()) :: binary
  def encrypt(packet) do
    bytes =
      packet
      |> to_charlist
      |> Enum.with_index()

    length = length(bytes)
    data = for {b, i} <- bytes, into: <<>>, do: do_encrypt(b, i, length)
    <<data::binary, 0xFF::size(8)>>
  end

  @doc """
  Decrypt the first packet who contains the session_id.
  """
  @spec decrypt_session(binary) :: String.t()
  def decrypt_session(<<_::size(8), payload::binary>>) do
    payload
    |> do_decrypt_session()
    |> String.split()
    |> Enum.at(1)
  end

  @doc """
  Decrypt a world packet.

  ## Examples

  """
  @spec decrypt(binary, integer, boolean) :: [binary | {integer, binary}]
  def decrypt(binary, session_id, keepalive? \\ false) do
    session_key = session_id &&& 0xFF
    offset = session_key + 0x40 &&& 0xFF
    switch = session_id >>> 6 &&& 0x03

    packets =
      for <<c <- binary>>, into: <<>> do
        char =
          case switch do
            0 -> c - offset
            1 -> c + offset
            2 -> (c - offset) ^^^ 0xC3
            3 -> (c + offset) ^^^ 0xC3
          end

        <<char::size(8)>>
      end

    result =
      packets
      |> :binary.split(<<0xFF>>, [:global, :trim_all])
      |> Enum.map(&do_decrypt/1)

    case keepalive? do
      false ->
        result

      true ->
        result
        |> Stream.map(&String.split(&1, " ", parts: 2))
        |> Enum.map(fn [l, r] -> {String.to_integer(l), r} end)
    end
  end

  #
  # Private functions
  #

  @doc false
  @spec do_encrypt(char, integer, integer) :: binary
  defp do_encrypt(char, index, _) when rem(index, 0x7E) != 0, do: <<(~~~char)>>

  defp do_encrypt(char, index, length) do
    remaining = if length - index > 0x7E, do: 0x7E, else: length - index
    <<remaining::size(8), ~~~char::size(8)>>
  end

  @doc false
  @spec do_decrypt_session(binary, String.t()) :: String.t()
  defp do_decrypt_session(binary, result \\ "")
  defp do_decrypt_session(<<>>, result), do: result
  defp do_decrypt_session(<<0xE::size(8), _::binary>>, result), do: result

  defp do_decrypt_session(<<char::size(8), rest::binary>>, result) do
    first_byte = char - 0xF
    second_byte = first_byte &&& 0xF0
    first_key = first_byte - second_byte
    second_key = second_byte >>> 0x4

    first =
      case second_key do
        0 -> " "
        1 -> " "
        2 -> "-"
        3 -> "."
        _ -> <<0x2C + second_key::utf8>>
      end

    second =
      case first_key do
        0 -> " "
        1 -> " "
        2 -> "-"
        3 -> "."
        _ -> <<0x2C + first_key::utf8>>
      end

    do_decrypt_session(rest, result <> first <> second)
  end

  @doc false
  @spec do_decrypt(binary, list) :: String.t()
  defp do_decrypt(binary, result \\ [])
  defp do_decrypt("", result), do: result |> Enum.reverse() |> Enum.join()

  defp do_decrypt(<<byte::size(8), rest::binary>>, result) when byte <= 0x7A do
    len = Enum.min([byte, byte_size(rest)])
    {first, second} = String.split_at(rest, len)
    res = for <<c <- first>>, into: "", do: <<c ^^^ 0xFF>>
    do_decrypt(second, [res | result])
  end

  defp do_decrypt(<<byte::size(8), rest::binary>>, result) do
    len = byte &&& 0x7F
    {first, second} = do_decrypt2(rest, len)
    do_decrypt(second, [first | result])
  end

  @spec do_decrypt2(binary, integer, binary) :: {binary, binary}
  defp do_decrypt2(bin, len, i \\ 0, result \\ "")
  defp do_decrypt2("", _, _, result), do: {result, ""}
  defp do_decrypt2(bin, len, i, result) when i >= len, do: {result, bin}

  defp do_decrypt2(bin, len, i, result) do
    <<h::size(4), l::size(4), rest::binary>> = bin

    res =
      cond do
        h != 0 and h != 0xF and (l == 0 or l == 0xF) ->
          Enum.at(@table, h - 1)

        l != 0 and l != 0xF and (h == 0 or h == 0xF) ->
          Enum.at(@table, l - 1)

        h != 0 and h != 0xF and l != 0 and l != 0xF ->
          Enum.at(@table, h - 1) <> Enum.at(@table, l - 1)

        true ->
          ""
      end

    case h != 0 and h != 0xF do
      true -> do_decrypt2(rest, len, i + 2, result <> res)
      false -> do_decrypt2(rest, len, i + 1, result <> res)
    end
  end
end
