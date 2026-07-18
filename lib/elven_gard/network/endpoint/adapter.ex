defmodule ElvenGard.Network.Endpoint.Adapter do
  @moduledoc """
  Defines the listener operations required by `ElvenGard.Network.Endpoint`.

  Endpoint adapters translate generic endpoint configuration into the
  server-specific listener lifecycle.
  """

  alias ElvenGard.Network.Endpoint

  ## Callbacks

  @callback child_spec(endpoint :: module(), config :: Endpoint.config()) ::
              Supervisor.child_spec()

  @callback get_addr(endpoint :: module(), config :: Endpoint.config()) :: String.t()

  @callback get_port(endpoint :: module(), config :: Endpoint.config()) ::
              :inet.port_number()
end
