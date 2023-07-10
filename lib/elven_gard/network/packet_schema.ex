defmodule ElvenGard.Network.PacketSchema do
  @moduledoc ~S"""
  ElvenGard.Network.PacketSchema
  """

  ## Helpers

  defguardp is_packet_id(id) when is_integer(id) or is_binary(id)

  ## Public API

  # packet 0x0000
  defmacro packet(id) when is_packet_id(id) do
    do_packet(id, id_to_name(id), true, nil)
  end

  # packet 0x0000 when ...
  defmacro packet({:when, _, [id, guards]}) when is_packet_id(id) do
    do_packet(id, id_to_name(id), guards, nil)
  end

  # packet 0x0000, as: ModuleName
  defmacro packet(id, as: name) when is_packet_id(id) do
    do_packet(id, name, true, nil)
  end

  # packet 0x0000 when ..., as: ModuleName
  defmacro packet({:when, _, [id, guards]}, as: name) when is_packet_id(id) do
    do_packet(id, name, guards, nil)
  end

  # packet 0x0000 do ... end
  defmacro packet(id, do: exp) when is_packet_id(id) do
    do_packet(id, id_to_name(id), true, exp)
  end

  # packet 0x0000 when ... do ... end
  defmacro packet({:when, _, [id, guards]}, do: exp) when is_packet_id(id) do
    do_packet(id, id_to_name(id), guards, exp)
  end

  # packet 0x0000, as: ModuleName do ... end
  defmacro packet(id, [as: name], do: exp) when is_packet_id(id) do
    do_packet(id, name, true, exp)
  end

  # packet 0x0000 when ..., as: ModuleName do ... end
  defmacro packet({:when, _, [id, guards]}, [as: name], do: exp) when is_packet_id(id) do
    do_packet(id, name, guards, exp)
  end

  # field :protocol_version, VarInt
  defmacro field(name, type, opts \\ []) do
    do_field(name, type, opts)
  end

  defmacro __using__(_env) do
    quote location: :keep do
      import unquote(__MODULE__),
        only: [
          packet: 1,
          packet: 2,
          packet: 3,
          field: 2,
          field: 3
        ]
    end
  end

  ## Private funtions

  defp id_to_name(id) when is_integer(id), do: :"Elixir.Packet#{id}"

  defp id_to_name(id) when is_binary(id) do
    {h, t} = String.split_at(id, 1)
    :"Elixir.#{String.upcase(h)}#{t}"
  end

  defp do_packet(id, name, guards, exp) do
    quote location: :keep, generated: true do
      def decode(var!(packet_id) = unquote(id), var!(data), var!(socket))
          when unquote(guards) do
        var!(packet) = %{packet_id: unquote(id), packet_name: unquote(name)}

        unquote(exp)

        if var!(data) != "" do
          iname = inspect(unquote(name))
          raise "remaining bytes for #{inspect(__MODULE__)}.#{iname}: - #{inspect(var!(data))}"
        end

        var!(packet)
      end
    end
  end

  defp do_field(name, type, opts) do
    {condition, opts} = Keyword.pop(opts, :if)

    if is_nil(condition) do
      quote location: :keep, generated: true do
        {value, var!(data)} = unquote(type).decode(var!(data), unquote(opts))
        var!(packet) = Map.put(var!(packet), unquote(name), value)
      end
    else
      quote location: :keep, generated: true do
        {value, var!(data)} =
          case unquote(condition) do
            result when result in [false, nil] -> {nil, var!(data)}
            _ -> unquote(type).decode(var!(data), unquote(opts))
          end

        var!(packet) = Map.put(var!(packet), unquote(name), value)
      end
    end
  end
end
