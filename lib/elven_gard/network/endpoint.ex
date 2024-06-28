defmodule ElvenGard.Network.Endpoint do
  @moduledoc ~S"""
  TODO: Documentation for ElvenGard.Network.Endpoint.
  """

  @typedoc """
  Possible options to configure a server. Valid option values are as follows:

  * `aaaa`: bbbb. Required
  * `xxxx`: yyyyy.
  """
  @type options :: [
          adapter: module(),
          adapter_options: any(),
          protocol: module(),
          ip_address: String.t(),
          port: :inet.port_number()
        ]

  ## Endpoint behaviour

  @callback start_link(options()) :: Supervisor.on_start()

  ## Public API

  @doc false
  @spec child_spec(options()) :: Supervisor.child_spec()
  def child_spec(opts) do
    default = [
      # adapter: ElvenGard.Network.Endpoint.Ranch,
      adapter: ElvenGard.Network.Endpoint.ThousandIsland,
      adapter_options: nil,
      ip_address: "127.0.0.1"
    ]

    opts = Keyword.merge(default, opts)

    %{
      id: {__MODULE__, make_ref()},
      start: {opts[:adapter], :start_link, [opts]},
      restart: :permanent
    }
  end
end
