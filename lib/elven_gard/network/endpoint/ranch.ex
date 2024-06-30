if Code.ensure_loaded?(:ranch) do
  defmodule ElvenGard.Network.Endpoint.Ranch do
    @moduledoc """
    Endpoint adapter using Ranch
    """

    require Logger

    alias ElvenGard.Network.Endpoint

    @behaviour ElvenGard.Network.Endpoint

    ## Endpoint behaviour

    @impl true
    def start_link(opts) do
      Logger.info(build_info(opts))
      Supervisor.start_link([{__MODULE__.Listener, opts}], strategy: :one_for_one)
    end

    ## Private functions

    defp build_info(opts) do
      server = "Ranch #{Application.spec(:ranch)[:vsn]}"
      Endpoint.build_info(server, opts)
    end
  end

  defmodule ElvenGard.Network.Endpoint.Ranch.Listener do
    @moduledoc false

    alias ElvenGard.Network.Endpoint

    ## Public API

    @spec child_spec(Endpoint.options()) :: :supervisor.child_spec()
    def child_spec(opts) do
      socket_handler = Keyword.fetch!(opts, :socket_handler)

      :ranch.child_spec(
        {__MODULE__, socket_handler},
        transport_module!(opts),
        transport_options!(opts),
        socket_handler,
        []
      )
    end

    ## Private API

    defp transport_module!(opts) do
      case Keyword.get(opts, :transport) do
        :tcp -> :ranch_tcp
        :ssl -> :ranch_ssl
        transport -> raise "transport #{inspect(transport)} not supported by Ranch"
      end
    end

    defp transport_options!(opts) do
      port = Keyword.fetch!(opts, :port)

      ip =
        opts
        |> Keyword.fetch!(:ip)
        |> String.to_charlist()
        |> :inet.parse_address()
        |> then(fn {:ok, tuple} -> tuple end)

      socket_opts_access = Access.key(:socket_opts, [])

      opts
      |> Keyword.get(:transport_options, [])
      |> Map.new()
      |> put_in([socket_opts_access, :ip], ip)
      |> put_in([socket_opts_access, :port], port)
    end
  end
end
