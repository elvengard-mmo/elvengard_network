defmodule ElvenGard.Network.View do
  @moduledoc ~S"""
  Define a custom behaviour for a view (packet sent from server to client)
  """

  alias ElvenGard.Network.UnknownViewError

  @doc """
  Build a packet to send to the client
  """
  @callback render(name, params) :: iodata()
            when name: atom() | String.t(), params: map() | Keyword.t()

  @doc false
  defmacro __using__(_) do
    quote do
      @behaviour unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    # We are using `generated: true` because we don't want warnings coming
    # from `c:render/2` to be reported in case the user has defined a
    # catch-all `c:render/2` clause.
    quote generated: true do
      def render(type, _args) do
        raise UnknownViewError, parent: unquote(__CALLER__.module), type: type
      end
    end
  end
end
