defmodule ElvenGard.Helpers.View do
  @moduledoc """
  Define a custom behaviour for a view (packet sent from server to client)
  """

  @callback render(type :: atom, opts :: map) :: term

  @doc false
  defmacro __using__(_) do
    parent = __MODULE__

    quote do
      @behaviour unquote(parent)
    end
  end
end
