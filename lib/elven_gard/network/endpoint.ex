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
          adapter_options: list(),
          socket_handler: module(),
          ip: String.t(),
          port: :inet.port_number(),
          transport: :tcp | :ssl | atom(),
          transport_options: list()
        ]

  ## Endpoint behaviour

  @callback start_link(options()) :: Supervisor.on_start()
  @callback setopts(state, opts) :: :ok | {:error, :inet.posix()}
            when state: any(), opts: [:inet.socket_setopt()]

  ## Public API

  @doc false
  @spec child_spec(options()) :: Supervisor.child_spec()
  def child_spec(opts) do
    default = [
      adapter: ElvenGard.Network.Endpoints.Ranch,
      # adapter: ElvenGard.Network.Endpoints.ThousandIsland,
      adapter_options: [],
      ip: "127.0.0.1",
      transport: :tcp,
      transport_options: []
    ]

    opts = Keyword.merge(default, opts)

    _port = required_opt!(opts, :port)
    _socket_handler = required_opt!(opts, :socket_handler)

    %{
      id: {__MODULE__, make_ref()},
      start: {opts[:adapter], :start_link, [opts]},
      restart: :permanent
    }
  end

  @doc false
  @spec build_info(String.t(), options()) :: String.t()
  def build_info(server, opts) do
    ip = Keyword.fetch!(opts, :ip)
    port = Keyword.fetch!(opts, :port)
    socket_handler = Keyword.fetch!(opts, :socket_handler)
    transport = Keyword.fetch!(opts, :transport)

    bind = "#{ip}:#{port}"
    "Running #{inspect(socket_handler)} with #{server} at #{bind} (#{transport})"
  end

  ## Private function

  defp required_opt!(opts, name) do
    opts[name] || raise ArgumentError, "#{inspect(name)} option is required"
  end
end
