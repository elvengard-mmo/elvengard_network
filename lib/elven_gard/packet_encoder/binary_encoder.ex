defmodule ElvenGard.PacketEncoder.BinaryEncoder do
  @moduledoc false

  alias ElvenGard.Structures.{Client, PacketDefinition}

  @aliases [
    byte: ElvenGard.Types.Binary.ByteType,
    integer: ElvenGard.Types.Binary.IntegerType,
    long: ElvenGard.Types.Binary.LongType,
    padding: ElvenGard.Types.Binary.PaddingType,
    short: ElvenGard.Types.Binary.ShortType,
    string: ElvenGard.Types.Binary.StringType
  ]

  @doc false
  defmacro __using__(model: model) do
    expanded_model = Macro.expand(model, __CALLER__)
    defs = expanded_model.get_packet_definitions()

    check_types!(defs)

    quote do
      use ElvenGard.Helpers.BasicEncoder

      @impl true
      def aliases() do
        unquote(@aliases)
      end

      ## Principal decoder
      def complete_decode(data, %Client{} = client) do
        data
        |> pre_decode(client)
        |> decode()
        |> post_decode(client)
        |> binary_decode()
      end

      ## Define sub decoders
      Enum.each(unquote(model).get_packet_definitions(), fn packet ->
        name = packet.name
        fields = Macro.escape(packet.fields)

        contents =
          quote do
            defp binary_decode({unquote(name), params}) do
              res = do_binary_decode(params, unquote(fields), %{})
              {unquote(name), res}
            end
          end

        Module.eval_quoted(__MODULE__, contents)
      end)

      @doc false
      @spec do_binary_decode(bitstring, list, map) :: map
      defp do_binary_decode(_bin, [], params), do: params

      defp do_binary_decode(bin, [field | tail_fields], params) do
        %{
          name: name,
          type: type,
          opts: opts
        } = field

        {val, rest} = type.decode(bin, opts)
        do_binary_decode(rest, tail_fields, Map.put(params, name, val))
      end
    end
  end

  @doc false
  @spec check_types!([PacketDefinition.t()]) :: term
  defp check_types!(defs) do
    for def <- defs, field <- def.fields do
      name = field.name
      type = field.type

      case Keyword.get(@aliases, type) do
        nil ->
          check_type!(type, name, def.name)

        real_type ->
          check_type!(real_type, name, def.name)
      end
    end
  end

  @doc false
  @spec check_type!(atom, atom, term) :: term
  defp check_type!(type, name, def_name) do
    unless Keyword.has_key?(type.__info__(:functions), :decode) do
      raise "Invalid type '#{inspect(type)}' for '#{inspect(name)}' " <>
              "for packet '#{inspect(def_name)}'"
    end
  end
end
