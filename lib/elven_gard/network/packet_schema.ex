defmodule ElvenGard.Network.PacketSchema do
  @moduledoc ~S"""
  Packet Schema DSL for defining received packet structures.

  This module provides a DSL (Domain-Specific Language) for defining packet schemas
  for received packets in a network protocol. It enables users to create structured
  packets with specified fields and decode binary data into packet structs.

  To learn more about the available macros and how to define packet schemas, refer
  to the [Packet Schema DSL documentation](<PACKETSCHEMA_URL>).

  ## Packets Macros

  The `packet` macros (`packet/1`, `packet/2` and `packet/3`) allow users to define
  packet structures.  
  They always require a packet ID. The alias (which is the name of the generated
  packet structure), the guard (with the `when` keyword), and do-block (for 
  defining fields) are optional.

  Users can specify guards in packets macros to conditionally match packets based
  on a condition (often using socket assigns).

  For more details on using the packets macros, please refer to the guide at
  <TODO: PACKETSCHEMA_URL> or to examples.

  ## Packet Structure and Decoding

  The `packets` macros generate both a packet structure and a `decode/3` function
  for each defined packet. The packet structure represents the decoded packet data,
  and the `decode/3` function takes binary data and converts it into the structure
  based on the defined schema.

  The `decode/3` function generated for each packet should be used by
  `c:ElvenGard.Network.PacketCodec.deserialize/2`.

  ## Field Macros

  The field macros are used to define fields within a packet:

  - `field/2`: Define a field with a name and type.
  - `field/3`: Define a field with a name, type, and decoding options.

  ## Examples

      defmodule MyApp.Endpoint.PacketSchemas do
        use ElvenGard.Network.PacketSchema

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

  @type packet_id :: integer() | binary()

  ## Helpers

  defguardp is_packet_id(id) when is_integer(id) or is_binary(id)

  ## Public API

  # packet 0x0000
  defmacro packet(id) when is_packet_id(id) do
    do_packet(id, id_to_name(id), nil, nil)
  end

  # packet 0x0000 when ...
  defmacro packet({:when, _, [id, guards]}) when is_packet_id(id) do
    do_packet(id, id_to_name(id), guards, nil)
  end

  # packet 0x0000, as: ModuleName
  defmacro packet(id, as: name) when is_packet_id(id) do
    do_packet(id, name, nil, nil)
  end

  # packet 0x0000 when ..., as: ModuleName
  defmacro packet({:when, _, [id, guards]}, as: name) when is_packet_id(id) do
    do_packet(id, name, guards, nil)
  end

  # packet 0x0000 do ... end
  defmacro packet(id, do: exp) when is_packet_id(id) do
    do_packet(id, id_to_name(id), nil, exp)
  end

  # packet 0x0000 when ... do ... end
  defmacro packet({:when, _, [id, guards]}, do: exp) when is_packet_id(id) do
    do_packet(id, id_to_name(id), guards, exp)
  end

  # packet 0x0000, as: ModuleName do ... end
  defmacro packet(id, [as: name], do: exp) when is_packet_id(id) do
    do_packet(id, name, nil, exp)
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

      @before_compile unquote(__MODULE__)

      Module.register_attribute(__MODULE__, :egn_packet_fields, accumulate: true)
      Module.register_attribute(__MODULE__, :egn_packets, accumulate: true)
    end
  end

  defmacro __before_compile__(env) do
    egn_packets = Module.get_attribute(env.module, :egn_packets)

    egn_packets
    |> Enum.map(fn %{id: id, name: name, fields: fields, guards: guards} = packet ->
      quote location: :keep, generated: true do
        defmodule :"#{Module.concat(__MODULE__, unquote(name))}" do
          # Structure

          defstruct Enum.map(unquote(Macro.escape(fields)), & &1.name)

          # Introspection

          @doc false
          def __schema__(:id), do: unquote(id)
          def __schema__(:name), do: unquote(name)
          def __schema__(:guards), do: unquote(Macro.escape(guards))
          def __schema__(:fields), do: unquote(Macro.escape(fields))
        end

        # Decode term to struct

        unquote(decode_ast(packet))
      end
    end)
    |> Kernel.++([schemas_introspection()])
  end

  ## Private funtions

  defp id_to_name(id) when is_binary(id), do: :"Elixir.#{Macro.camelize(id)}"

  defp id_to_name(id) when is_integer(id) do
    raise "a module name is required for integer packet ids"
  end

  defp do_packet(id, name, guards, exp) do
    quote location: :keep do
      Module.delete_attribute(__MODULE__, :egn_packet_fields)

      unquote(exp)

      @egn_packets %{
        id: unquote(id),
        name: unquote(name),
        guards: unquote(Macro.escape(guards)),
        fields: Enum.reverse(@egn_packet_fields)
      }
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

  defp schemas_introspection() do
    quote do
      @doc false
      def __schemas__(), do: @egn_packets
    end
  end

  defp decode_ast(packet) do
    %{id: id, name: name, fields: fields, guards: guards} = packet
    guards = if is_nil(guards), do: true, else: guards
    fields_ast = fields |> Enum.map(&field_ast(&1, &1[:opts][:if])) |> merge_ast_blocks()

    quote location: :keep, generated: true do
      def decode(var!(packet_id) = unquote(id), var!(data), var!(socket)) when unquote(guards) do
        var!(packet) = struct(Module.concat(__MODULE__, unquote(name)), packet_id: unquote(id))

        unquote(fields_ast)

        if var!(data) != "" do
          iname = inspect(unquote(name))
          raise "remaining bytes for #{inspect(__MODULE__)}.#{iname}: #{inspect(var!(data))}"
        end

        var!(packet)
      end
    end
  end

  defp field_ast(%{name: name, type: type, opts: opts}, nil) do
    quote location: :keep, generated: true do
      {value, var!(data)} = unquote(type).decode(var!(data), unquote(opts))
      var!(packet) = Map.put(var!(packet), unquote(name), value)
    end
  end

  defp field_ast(%{name: name, type: type, opts: opts}, condition) do
    quote location: :keep, generated: true do
      {value, var!(data)} =
        case unquote(condition) do
          result when result in [false, nil] -> {nil, var!(data)}
          _ -> unquote(type).decode(var!(data), unquote(opts))
        end

      var!(packet) = Map.put(var!(packet), unquote(name), value)
    end
  end

  defp merge_ast_blocks(blocks) do
    blocks
    |> Macro.prewalk(fn
      {:__block__, _, block} -> block
      ast -> ast
    end)
    |> List.flatten()
    |> then(&{:__block__, [], &1})
  end
end
