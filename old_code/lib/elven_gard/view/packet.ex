defmodule ElvenGard.View.Packet do
  @moduledoc ~S"""
  TODO: Documentation
  """

  ## Callbacks

  @callback serialize(data :: any(), opts :: Keyword.t()) :: iodata()

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)
      @before_compile unquote(__MODULE__)

      import ElvenGard.Socket.Serializer, only: [serialize: 1, serialize: 2]

      unquote(impl(__CALLER__.module))
    end
  end

  ## Internal macros

  @doc false
  defmacro __before_compile__(env) do
    unless Module.defines?(env.module, {:__struct__, 0}) do
      raise ArgumentError, """
      the view `#{inspect(env.module)}` must define a structure.

      Example:

        defstruct [field: :default_value]

        @type t :: %#{inspect(env.module)}{field: atom()}

      """
    end

    unless Kernel.Typespec.defines_type?(env.module, {:t, 0}) do
      message = """
        no typespec found for the `#{inspect(env.module)}` structure.

        We will inject a default implementation for now:

          @opaque t :: %#{inspect(env.module)}{}
      """

      IO.warn(message, Macro.Env.stacktrace(env))

      quote do
        @opaque t :: %__MODULE__{}
      end
    end
  end

  ## Private functions

  @doc false
  defp impl(mod) do
    quote do
      defimpl ElvenGard.Socket.SerializerProtocol do
        def serialize(data, opts) do
          unquote(mod).serialize(data, opts)
        end
      end
    end
  end
end
