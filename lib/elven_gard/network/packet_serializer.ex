defmodule ElvenGard.Network.PacketSerializer do
  @moduledoc ~S"""
  Packet Serializer DSL for defining packet structures.

  This module provides a DSL (Domain-Specific Language) for defining packet serializers
  for both received and sent packets in a network protocol. It enables users to create
  structured packets with specified fields and decode binary data into packet structs.

  To learn more about the available macros and how to define packet serializers, refer
  to the [Packet Serializer DSL guide](https://hexdocs.pm/elvengard_network/packet_definitions.html).

  ## Packet Macros

  The `defpacket` macros (`defpacket/1`, `defpacket/2`, and `defpacket/3`) allow users
  to define packet structures. They require a packet ID and an alias (which is the name
  of the generated packet structure). The guard (with the `when` keyword) and do-block
  (for defining fields) are optional.

  Users can specify guards in packet macros to conditionally match packets based on a
  condition (often using socket assigns).

  ## Packet Structure, Serialization, and Deserialization

  The `defpacket` macros can generate a packet structure, a `deserialize/3` function for
  deserializing binary data into the packet structure, and a `serialize/1` function for
  generating the serialized binary representation of the packet.

  The `deserialize/3` function should be used for decoding received packets, and the
  `serialize/1` function should be used for generating packets to be sent over the network.

  ## Field Macros

  The field macros are used to define fields within a packet:

  - `field/2`: Define a field with a name and type.
  - `field/3`: Define a field with a name, type, and decoding options.

  ## Decorators

  The following decorators can be used to specify serialization and deserialization properties:

  - `@serializable true`: Marks the packet as serializable for sending over the network.
  - `@deserializable true`: Marks the packet as deserializable for receiving from the network.

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
    deserialize_body_fun = fn %{mod: mod} ->
      quote location: :keep do
        unquote(mod).deserialize(var!(packet_id), var!(data), var!(socket))
      end
    end

    mod = expand_aliases(mod, __CALLER__)
    deserialize_ast = Enum.map(mod.__schemas__(), &def_deserialize(&1, deserialize_body_fun))

    quote location: :keep do
      (unquote_splicing(deserialize_ast))
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
    deserialize_body_fun = fn %{id: id, mod: mod} ->
      quote do
        unquote(mod).deserialize(unquote(id), var!(data), var!(socket))
      end
    end

    egn_packets = Module.get_attribute(env.module, :egn_packets)
    deserialize_functions = Enum.map(egn_packets, &def_deserialize(&1, deserialize_body_fun))

    quote do
      unquote(schemas_introspection())
      unquote_splicing(deserialize_functions)
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
    exp = Macro.postwalk(exp, &Macro.expand(&1, caller))

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

  defp def_serialize(%{id: id, fields: fields}) do
    fields_ast =
      Enum.map(fields, fn %{name: name, type: type, opts: opts} ->
        quote location: :keep do
          case {Map.fetch!(var!(packet), unquote(name)), opts[:default]} do
            {nil, value} when not is_nil(value) -> value
            {value, _} -> unquote(type).encode(value, unquote(opts))
          end
        end
      end)

    quote location: :keep, generated: true do
      def serialize(%__MODULE__{} = var!(packet)) do
        {unquote(id), unquote(fields_ast)}
      end
    end
  end

  defp def_deserialize(%{id: id, guards: guards} = packet, body_cb) do
    guards = if is_nil(guards), do: true, else: guards

    quote location: :keep, generated: true do
      def deserialize(var!(packet_id) = unquote(id), var!(data), var!(socket))
          when unquote(guards) do
        unquote(body_cb.(packet))
      end
    end
  end

  # credo:disable-for-next-line
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

    deserialize_body_fun = fn %{name: name} ->
      quote location: :keep do
        var!(packet) = %{}

        unquote_splicing(fields_ast)

        if var!(data) != "" do
          iname = inspect(unquote(name))
          raise "remaining bytes for #{inspect(__MODULE__)}.#{iname}: #{inspect(var!(data))}"
        end

        struct!(__MODULE__, var!(packet))
      end
    end

    quote location: :keep do
      defmodule unquote(mod) do
        import unquote(parent), except: [deserialize: 3, serialize: 1]

        # Structure

        @enforce_keys for f <- unquote(Macro.escape(fields)),
                          is_nil(f[:opts][:default]),
                          do: f.name
        defstruct Enum.map(unquote(Macro.escape(fields)), &{&1.name, &1[:opts][:default]})

        # Introspection

        @doc false
        def __schema__(:id), do: unquote(id)
        def __schema__(:name), do: unquote(name)
        def __schema__(:guards), do: unquote(Macro.escape(guards))
        def __schema__(:fields), do: unquote(Macro.escape(fields))

        # Serializer/Deserializer

        if unquote(serializable) do
          unquote(def_serialize(packet))
        else
          def serialize(_), do: raise("unimplemented")
        end

        if unquote(deserializable) do
          unquote(def_deserialize(packet, deserialize_body_fun))
        else
          def deserialize(_, _, _), do: raise("unimplemented")
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
