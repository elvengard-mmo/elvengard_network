defmodule ElvenGard.View do
  @moduledoc """
  Define a custom behaviour for a view (packet sent from server to client)
  """

  alias ElvenGard.UnknownViewError

  @anno if :erlang.system_info(:otp_release) >= '19', do: [generated: true], else: [line: -1]

  @doc """
  Build a packet to send to the client
  """
  @callback render(type :: atom, opts :: map) :: term

  @doc false
  defmacro __using__(_) do
    parent = __MODULE__

    quote do
      @behaviour unquote(parent)

      @before_compile unquote(parent)
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    parent = __CALLER__.module

    # We are using @anno because we don't want warnings coming from
    # `c:render/2` to be reported in case the user has defined a catch-all
    # `c:render/2` clause.
    #
    # Thanks to Phoenix
    # https://github.com/phoenixframework/phoenix/blob/master/lib/phoenix/view.ex
    quote @anno do
      def render(type, _args) do
        raise UnknownViewError, parent: unquote(parent), type: type
      end
    end
  end
end
