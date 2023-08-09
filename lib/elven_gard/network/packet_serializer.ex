defmodule ElvenGard.Network.PacketSerializer do
  @moduledoc ~S"""
  Packet Serializer DSL for defining received packet structures.

  This module provides a DSL (Domain-Specific Language) for defining packet serializers
  for received packets in a network protocol. It enables users to create structured
  packets with specified fields and decode binary data into packet structs.

  To learn more about the available macros and how to define packet serializers, refer
  to the [Packet Serializer DSL documentation](<PACKETSERIALIZER_URL>).

  ## Packets Macros

  The `packet` macros (`defpacket/1`, `defpacket/2` and `defpacket/3`) allow users to define
  packet structures.
  They always require a packet ID. The alias (which is the name of the generated
  packet structure), the guard (with the `when` keyword), and do-block (for
  defining fields) are optional.

  Users can specify guards in packets macros to conditionally match packets based
  on a condition (often using socket assigns).

  For more details on using the packets macros, please refer to the guide at
  <TODO: PACKETSERIALIZER_URL> or to examples.

  ## Packet Structure and Decoding

  The `packets` macros generate both a packet structure and a `decode/3` function
  for each defined packet. The packet structure represents the decoded packet data,
  and the `decode/3` function takes binary data and converts it into the structure
  based on the defined schema.

  The `decode/3` function generated for each packet should be used by
  `c:ElvenGard.Network.NetworkCodec.deserialize/2`.

  ## Field Macros

  The field macros are used to define fields within a packet:

  - `field/2`: Define a field with a name and type.
  - `field/3`: Define a field with a name, type, and decoding options.

  ## Examples

      defmodule MyApp.Endpoint.PacketSerializers do
        use ElvenGard.Network.PacketSerializer

        alias MyApp.Types.{Integer, String}

        # Simple string packet:
        #   - This generate a SimplePacket structure
        #   - The `decode/3` function will match all packet with `simple_packet` as packet id
        #   - The structure will have 2 fields: `id` and `name` (a string and an integer)
        packet "simple_packet" do
          field :id, Integer
          field :name, String
        end

        # Complex string packet:
        #   - This generate a AliasedModule structure
        #   - The `decode/3` function will match packet with `complex_packet` as packet id ONLY if the state is `:init`
        #   - The structure will have 2 fields: `id` and `name` (a string and an integer)
        #   - We're also passing `[fill: true]` as option to our `MyApp.Types.String.decode/2`
        packet "complex_packet" when socket.assigns.state == :init, as: AliasedModule do
          field :id, Integer
          field :name, String, fill: true
        end

        # Simple binary packet
        #   - This generate a KeepAlivePacket structure
        #   - The `decode/3` function will match packet with `0x0000` as packet id
        #   - The structure will have no field
        #   - Here the `:as` option is required because the packet ID is an integer
        packet 0x0000, as: KeepAlivePacket

  """

  ## Helpers

  defguardp is_packet_id(id) when is_integer(id) or is_binary(id)

  ## Public API

  # defpacket 0x0000
  defmacro defpacket(id) when is_packet_id(id) do
    do_packet(id, id_to_name(id), nil, nil, __CALLER__)
  end

  # defpacket 0x0000 when ...
  defmacro defpacket({:when, _, [id, guards]}) when is_packet_id(id) do
    do_packet(id, id_to_name(id), guards, nil, __CALLER__)
  end

  # defpacket 0x0000, as: ModuleName
  defmacro defpacket(id, as: name) when is_packet_id(id) do
    do_packet(id, name, nil, nil, __CALLER__)
  end

  # defpacket 0x0000 when ..., as: ModuleName
  defmacro defpacket({:when, _, [id, guards]}, as: name) when is_packet_id(id) do
    do_packet(id, name, guards, nil, __CALLER__)
  end

  # defpacket 0x0000 do ... end
  defmacro defpacket(id, do: exp) when is_packet_id(id) do
    do_packet(id, id_to_name(id), nil, exp, __CALLER__)
  end

  # defpacket 0x0000 when ... do ... end
  defmacro defpacket({:when, _, [id, guards]}, do: exp) when is_packet_id(id) do
    do_packet(id, id_to_name(id), guards, exp, __CALLER__)
  end

  # defpacket 0x0000, as: ModuleName do ... end
  defmacro defpacket(id, [as: name], do: exp) when is_packet_id(id) do
    do_packet(id, name, nil, exp, __CALLER__)
  end

  # defpacket 0x0000 when ..., as: ModuleName do ... end
  defmacro defpacket({:when, _, [id, guards]}, [as: name], do: exp) when is_packet_id(id) do
    do_packet(id, name, guards, exp, __CALLER__)
  end

  # field :protocol_version, VarInt
  defmacro field(name, type, opts \\ []) do
    do_field(name, type, opts)
  end

  defmacro import_packets(mod) do
    decode_body_fun = fn %{mod: mod} ->
      quote location: :keep do
        unquote(mod).decode(var!(packet_id), var!(data), var!(socket))
      end
    end

    mod = expand_aliases(mod, __CALLER__)
    decode_ast = Enum.map(mod.__schemas__(), &def_decode(&1, decode_body_fun))

    quote location: :keep do
      (unquote_splicing(decode_ast))
    end
  end

  defmacro __using__(_env) do
    quote location: :keep do
      import unquote(__MODULE__),
        only: [
          defpacket: 1,
          defpacket: 2,
          defpacket: 3,
          field: 2,
          field: 3
        ]

      @before_compile unquote(__MODULE__)
      @after_compile unquote(__MODULE__)

      Module.register_attribute(__MODULE__, :egn_packet_fields, accumulate: true)
      Module.register_attribute(__MODULE__, :egn_packets, accumulate: true)

      @serializable false
      @deserializable false
    end
  end

  defmacro __before_compile__(env) do
    decode_body_fun = fn %{id: id, mod: mod} ->
      quote do
        unquote(mod).decode(unquote(id), var!(data), var!(socket))
      end
    end

    egn_packets = Module.get_attribute(env.module, :egn_packets)
    decode_functions = Enum.map(egn_packets, &def_decode(&1, decode_body_fun))

    quote do
      unquote(schemas_introspection())
      unquote_splicing(decode_functions)
    end
  end

  defmacro __after_compile__(env, _bytecode) do
    env.module
    |> Module.get_attribute(:egn_packets)
    |> Enum.map(&def_structure/1)
    |> Code.compile_quoted()
  end

  ## Private funtions

  defp id_to_name(id) when is_binary(id), do: :"Elixir.#{Macro.camelize(id)}"

  defp id_to_name(id) when is_integer(id) do
    raise "a module name is required for integer packet ids"
  end

  defp expand_aliases(ast, env) do
    Macro.postwalk(ast, fn
      {:__aliases__, _, _} = alias_ast -> Macro.expand(alias_ast, env)
      ast -> ast
    end)
  end

  defp schemas_introspection() do
    quote do
      @doc false
      def __schemas__(), do: @egn_packets
    end
  end

  defp do_packet(id, name, guards, exp, caller) do
    guard_env = %{caller | context: :guard}
    guards = Macro.postwalk(guards, &Macro.expand(&1, guard_env))
    exp = expand_aliases(exp, caller)

    quote location: :keep do
      if not @serializable and not @deserializable do
        mod = Module.concat(__MODULE__, unquote(name))
        IO.warn("packet #{inspect(mod)} defined but not serializable nor deserializable")
      end

      Module.delete_attribute(__MODULE__, :egn_packet_fields)

      unquote(exp)

      @egn_packets %{
        id: unquote(id),
        name: unquote(name),
        parent: __MODULE__,
        mod: Module.concat(__MODULE__, unquote(name)),
        guards: unquote(Macro.escape(guards)),
        fields: Enum.reverse(@egn_packet_fields),
        serializable: @serializable,
        deserializable: @deserializable
      }

      @serializable false
      @deserializable false
    end
  end

  defp do_field(name, type, opts) do
    quote location: :keep do
      @egn_packet_fields %{
        name: unquote(name),
        type: unquote(type),
        opts: unquote(Macro.escape(opts))
      }
    end
  end

  defp def_encode(%{id: id, fields: fields}) do
    fields_ast =
      Enum.map(fields, fn %{name: name, type: type, opts: opts} ->
        quote location: :keep do
          unquote(type).encode(
            Map.fetch!(var!(packet), unquote(name)),
            unquote(opts)
          )
        end
      end)

    quote location: :keep, generated: true do
      def encode(%__MODULE__{} = var!(packet)) do
        {unquote(id), unquote(fields_ast)}
      end
    end
  end

  defp def_decode(%{id: id, guards: guards} = packet, body_cb) do
    guards = if is_nil(guards), do: true, else: guards

    quote location: :keep, generated: true do
      def decode(var!(packet_id) = unquote(id), var!(data), var!(socket)) when unquote(guards) do
        unquote(body_cb.(packet))
      end
    end
  end

  defp def_structure(packet) do
    %{
      id: id,
      name: name,
      parent: parent,
      mod: mod,
      guards: guards,
      fields: fields,
      serializable: serializable,
      deserializable: deserializable
    } = packet

    fields_ast = Enum.map(fields, &decode_field_ast(&1, &1[:opts][:if]))

    decode_body_fun = fn %{name: name} ->
      quote location: :keep do
        var!(packet) = %__MODULE__{}

        unquote_splicing(fields_ast)

        if var!(data) != "" do
          iname = inspect(unquote(name))
          raise "remaining bytes for #{inspect(__MODULE__)}.#{iname}: #{inspect(var!(data))}"
        end

        var!(packet)
      end
    end

    quote location: :keep do
      defmodule unquote(mod) do
        import unquote(parent), except: [decode: 3, encode: 3]

        # Structure

        defstruct Enum.map(unquote(Macro.escape(fields)), & &1.name)

        # Introspection

        @doc false
        def __schema__(:id), do: unquote(id)
        def __schema__(:name), do: unquote(name)
        def __schema__(:guards), do: unquote(Macro.escape(guards))
        def __schema__(:fields), do: unquote(Macro.escape(fields))

        # Encoder/Decoder

        if unquote(serializable) do
          unquote(def_encode(packet))
        else
          def encode(_), do: raise("unimplemented")
        end

        if unquote(deserializable) do
          unquote(def_decode(packet, decode_body_fun))
        else
          def decode(_, _, _), do: raise("unimplemented")
        end
      end
    end
  end

  defp decode_field_ast(%{name: name, type: type, opts: opts}, nil) do
    quote location: :keep do
      {value, var!(data)} = unquote(type).decode(var!(data), unquote(opts))
      var!(packet) = Map.put(var!(packet), unquote(name), value)
    end
  end

  defp decode_field_ast(%{name: name, type: type, opts: opts}, condition) do
    quote location: :keep do
      {value, var!(data)} =
        case unquote(condition) do
          result when result in [false, nil] -> {nil, var!(data)}
          _ -> unquote(type).decode(var!(data), unquote(opts))
        end

      var!(packet) = Map.put(var!(packet), unquote(name), value)
    end
  end
end
