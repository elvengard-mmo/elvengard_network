defmodule ElvenGard.Socket.TextualSerializer do
  @moduledoc ~S"""
  WARN: This module will soon be deprecated.
  """

  import ElvenGard.Socket.Serializer, only: [serialize: 2]

  @aliases [
    integer: ElvenGard.Protocol.Textual.IntegerType,
    float: ElvenGard.Protocol.Textual.FloatType,
    string: ElvenGard.Protocol.Textual.StringType
  ]

  ## Public API

  @doc false
  defmacro __using__(opts) do
    quote do
      @behaviour ElvenGard.Socket.Serializer
      # @behaviour unquote(__MODULE__)

      unquote(defs(opts))
      unquote(decoders(opts, __CALLER__))
    end
  end

  ## Private functions

  @doc false
  defp defs(opts) do
    separator = Keyword.fetch!(opts, :separator)
    module_opts = [separator: separator]

    quote location: :keep do
      @impl true
      def aliases(), do: unquote(@aliases)

      @impl true
      def encode!(data, opts), do: serialize(data, Keyword.merge(unquote(module_opts), opts))

      @impl true
      def decode!("", _assigns), do: []

      def decode!(message, _assigns) do
        message
        # |> handle_decode(assigns)
        |> String.split(["\r\n", "\n", "\r"])
        |> Enum.map(&do_decode!/1)
      end

      ## Helpers

      defp do_decode!(message) do
        case String.split(message, unquote(separator), trim: true) do
          [header] -> {header, %{}}
          [header | str_args] -> parse_args!(header, str_args)
        end
      end
    end
  end

  @doc false
  defp decoders(opts, caller) do
    packet_handler =
      opts
      |> Keyword.fetch!(:packet_handler)
      |> Macro.expand(caller)

    defs = packet_handler.__defs__()

    IO.inspect(defs)

    # Enum.map(fn x ->

    #   quote location: :keep do
    #     defp parse_args!(unquote(x), [username, password]) do
    #       {"LOGIN", %{"username" => username, "password" => password}}
    #     end
    #   end
    # end)

    quote location: :keep do
      defp parse_args!("LOGIN", [username, password]) do
        {"LOGIN", %{"username" => username, "password" => password}}
      end

      defp parse_args!("PING", [count]) do
        {"PING", %{"count" => String.to_integer(count)}}
      end

      defp parse_args!(header, args) do
        raise "unable to decode args for header #{inspect(header)} (got: #{inspect(args)})"
      end
    end
  end
end
