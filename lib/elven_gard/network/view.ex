defmodule ElvenGard.Network.View do
  @moduledoc ~S"""
  Define a custom behaviour for a View (packet sent from server to client).

  This module defines a behaviour that allows users to create custom packet views
  to be sent from the server to the client. By implementing the callback defined
  in this module, users can define how a specific packet view is rendered and
  converted into binary data for transmission.

  To implement a custom view, you need to define the `c:render/2` callback. The
  `c:render/2` function takes the name of the packet view and a map of parameters
  as input and returns the binary data to be sent to the client or a structure 
  that will be serialized by `ElvenGard.Network.PacketSerializer`.

  ## Example

  Here's an example of defining a custom view for rendering a login response
  packet:

      defmodule MyApp.LoginViews do
        use ElvenGard.Network.View
        
        alias MyApp.Server.LoginPackets.LoginResponse

        @impl ElvenGard.Network.View
        def render(:login_response, %{status: status, message: message}) do
          # Encode the `json` field before generating the LoginResponse struct
          json = Poison.encode!(message)
          %LoginResponse{status: status, json: json}
        end
      end

  In the above example, we defined a custom views module `MyApp.LoginViews` that
  implements the `c:render/2` callback for rendering a login response packet.
  """

  alias ElvenGard.Network.UnknownViewError

  @doc """
  Build a packet to send to the client.

  Arguments:

  - `name`: A unique identifier packet view (string or atom).
  - `params`: A map or keyword list of parameters to be used when rendering the view.

  The function should return an iodata or a structure to be sent to the client.
  """
  @callback render(name, params) :: iodata() | struct()
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
