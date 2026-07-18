defmodule ElvenGard.Network.Endpoint.Adapters.Ranch do
  @moduledoc """
  Ranch listener adapter for `ElvenGard.Network.Endpoint`.
  """

  alias ElvenGard.Network.Endpoint
  alias ElvenGard.Network.Endpoint.Protocol

  @behaviour Endpoint.Adapter

  ## Endpoint.Adapter callbacks

  @impl true
  @spec child_spec(module(), Endpoint.config()) :: Supervisor.child_spec()
  def child_spec(endpoint, config) do
    runtime_options = [
      otp_app: Keyword.fetch!(config, :otp_app),
      socket_handler: Keyword.fetch!(config, :socket_handler)
    ]

    :ranch.child_spec(
      listener_ref(endpoint, config),
      transport_module(config),
      ranch_options(config),
      Protocol.Ranch,
      runtime_options
    )
  end

  @impl true
  @spec get_addr(module(), Endpoint.config()) :: String.t()
  def get_addr(endpoint, config) do
    endpoint
    |> listener_ref(config)
    |> :ranch.get_addr()
    |> elem(0)
    |> :inet.ntoa()
    |> List.to_string()
  end

  @impl true
  @spec get_port(module(), Endpoint.config()) :: :inet.port_number()
  def get_port(endpoint, config) do
    endpoint
    |> listener_ref(config)
    |> :ranch.get_port()
  end

  ## Private function

  defp listener_ref(endpoint, config) do
    {endpoint, Keyword.fetch!(config, :listener_name)}
  end

  defp transport_module(config) do
    case Keyword.fetch!(config, :transport) do
      :tcp -> :ranch_tcp
      :ssl -> :ranch_ssl
    end
  end

  defp ranch_options(config) do
    adapter_options = Keyword.fetch!(config, :adapter_options)

    socket_options =
      Keyword.merge(
        Keyword.fetch!(config, :transport_options),
        ip: config |> Keyword.fetch!(:ip) |> normalize_ip!(),
        port: Keyword.fetch!(config, :port)
      )

    adapter_options
    |> Map.new()
    |> Map.put(:socket_opts, socket_options)
  end

  defp normalize_ip!(ip) do
    case ip do
      ip when is_tuple(ip) -> ip
      ip when is_binary(ip) -> parse_ip!(ip)
    end
  end

  defp parse_ip!(ip) do
    case ip |> String.to_charlist() |> :inet.parse_address() do
      {:ok, address} ->
        address

      {:error, reason} ->
        raise ArgumentError, "invalid IP address #{inspect(ip)}: #{inspect(reason)}"
    end
  end
end
