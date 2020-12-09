defmodule ElvenGard.Socket.TextualSerializer do
  @moduledoc ~S"""
  WARN: This module will soon be deprecated.
  """

  import ElvenGard.Socket.Serializer, only: [serialize: 2]

  # @doc """
  # Same as c:ElvenGard.Protocol.encode/2
  # """
  # @callback handle_encode(data :: any(), opts :: keyword()) :: iodata()

  # @doc """
  # TODO: Documentation
  # """
  # @callback handle_decode(data :: binary(), assigns :: map()) :: any()

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
    end
  end

  ## Private functions

  @doc false
  defp defs(opts) do
    module_opts = [separator: Keyword.fetch!(opts, :separator)]

    quote location: :keep do
      @impl true
      def aliases(), do: unquote(@aliases)

      @impl true
      def encode!(data, opts), do: serialize(data, Keyword.merge(unquote(module_opts), opts))

      @impl true
      def decode!(_message, _assigns), do: []
    end
  end
end
