defmodule ElvenGard.PacketEncoder.BinaryEncoder do
  @moduledoc false

  alias ElvenGard.Structures.Client

  @doc false
  defmacro __using__(model: model) do
    quote do
      use ElvenGard.Helpers.BasicEncoder

      ## Principal decoder
      def complete_decode(data, %Client{} = client) do
        data
        |> pre_decode(client)
        |> decode()
        |> post_decode(client)
        |> binary_decode()
      end

      ## Define sub decoders
      Enum.each(unquote(model).elven_get_packet_documentation(), fn packet ->
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
end
