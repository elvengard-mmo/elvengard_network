defmodule ElvenGard.PacketEncoder.TextualEncoder do
  @moduledoc false

  alias ElvenGard.Structures.Client

  @doc false
  defmacro __using__(model: model, separator: separator) do
    quote do
      use ElvenGard.Helpers.BasicEncoder

      require Logger

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
      Enum.each(unquote(model).elven_get_packet_documentation(), fn packet ->
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
        Logger.debug("Can't decode packet with header #{name}: not defined in model #{m}")
        {name, params}
      end

      @doc false
      @spec do_textual_decode(list({String.t(), FieldDocumentation.t()}), map) :: map
      defp do_textual_decode([], params), do: params

      defp do_textual_decode([{data, field} | tail], params) do
        %{
          name: name,
          type: type,
          opts: opts
        } = field

        val = type.decode(data, opts)
        do_textual_decode(tail, Map.put(params, name, val))
      end
    end
  end
end
