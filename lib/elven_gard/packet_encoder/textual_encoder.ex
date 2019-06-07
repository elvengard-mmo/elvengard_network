defmodule ElvenGard.PacketEncoder.TextualEncoder do
  @moduledoc false

  alias ElvenGard.Structures.{Client, PacketDefinition}

  @aliases [
    integer: ElvenGard.Types.Textual.IntegerType,
    float: ElvenGard.Types.Textual.FloatType,
    string: ElvenGard.Types.Textual.StringType
  ]

  @doc false
  defmacro __using__(model: model, separator: separator) do
    expanded_model = Macro.expand(model, __CALLER__)
    defs = expanded_model.get_packet_definitions()

    check_types!(defs)

    quote do
      use ElvenGard.Helpers.BasicEncoder

      require Logger

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
        |> pre_textual_decode()
      end

      @doc false
      @spec pre_textual_decode({term, map} | list({term, map})) :: term
      defp pre_textual_decode(x) when is_tuple(x), do: textual_decode(x)
      defp pre_textual_decode(x) when is_list(x), do: Enum.map(x, &textual_decode/1)

      ## Define sub decoders
      Enum.each(unquote(model).get_packet_definitions(), fn packet ->
        name = packet.name
        fields = Macro.escape(packet.fields)
        sep = unquote(separator) |> Macro.escape()

        contents =
          quote do
            defp textual_decode({unquote(name), params}) do
              sp_params = String.split(params, unquote(sep))

              # TODO: Maybe check if len(sp_params) == len(fields)

              data = Enum.zip(sp_params, unquote(fields))
              res = do_textual_decode(data, %{})
              {unquote(name), res}
            end
          end

        Module.eval_quoted(__MODULE__, contents)
      end)

      ## Default sub decoder
      defp textual_decode({name, params}) do
        m = unquote(model)

        Logger.debug(fn ->
          "Can't decode packet with header #{name}: not defined in model #{m}"
        end)

        {name, params}
      end

      @doc false
      @spec do_textual_decode(list({String.t(), FieldDefinition.t()}), map) :: map
      defp do_textual_decode([], params), do: params

      defp do_textual_decode([{data, field} | tail], params) do
        %{
          name: name,
          type: type,
          opts: opts
        } = field

        real_type = Keyword.get(unquote(@aliases), type, type)

        val = real_type.decode(data, opts)
        do_textual_decode(tail, Map.put(params, name, val))
      end
    end
  end

  @doc false
  @spec check_types!([PacketDefinition.t()]) :: term
  defp check_types!(defs) do
    for def <- defs, field <- def.fields do
      name = field.name
      type = field.type
      real_type = Keyword.get(unquote(@aliases), type, type)

      check_type!(real_type, name, def.name)
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
