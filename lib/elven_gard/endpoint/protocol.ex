defmodule ElvenGard.Endpoint.Protocol do
  @moduledoc """
  TODO: Documentation
  """

  @callback handle_init(state) ::
              {:ok, state}
              | {:ok, state, timeout() | :hibernate | {:continue, term()}}
            when state: any()

  @optional_callbacks handle_init: 1

  @doc false
  defmacro __using__(_) do
    quote do
      use GenServer

      @behaviour unquote(__MODULE__)

      @impl true
      def handle_init(state), do: {:ok, state}

      defoverridable handle_init: 1
    end
  end
end
