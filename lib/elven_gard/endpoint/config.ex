defmodule ElvenGard.Endpoint.Config do
  # Handles ElvenGard Endpoints configuration.
  #
  # This module is private to ElvenGard and should not be accessed
  # directly. The ElvenGard endpoint configuration can be accessed
  # at runtime using the `config/2` function (TODO).
  @moduledoc false

  ## Public API

  @doc """
  The endpoint configuration used at compile time.
  """
  def config(otp_app, endpoint) do
    config(otp_app, endpoint, defaults(otp_app, endpoint))
  end

  defp config(otp_app, endpoint, defaults) do
    from_env(otp_app, endpoint, defaults)
  end

  @doc """
  Reads the configuration for module from the given otp app.

  Useful to read a particular value at compilation time.
  """
  def from_env(otp_app, module, defaults) do
    config = fetch_config(otp_app, module)

    merge(defaults, config)
  end

  @doc """
  Take 2 keyword lists and merge them recursively.

  Used to merge configuration values into defaults.
  """
  def merge(a, b), do: Keyword.merge(a, b, &merger/3)

  ## Private API

  @doc false
  defp defaults(otp_app, module) do
    [
      otp_app: otp_app,

      # Ranch options
      listener_name: module,
      transport: :ranch_tcp,
      transport_opts: %{
        socket_opts: [
          ip: {127, 0, 0, 1},
          port: 3000
        ]
      },
      protocol: module,
      protocol_opts: []
    ]
  end

  @doc false
  defp fetch_config(otp_app, module) do
    case Application.fetch_env(otp_app, module) do
      {:ok, conf} -> conf
      :error -> []
    end
  end

  @doc false
  defp merger(k, v1, v2) do
    cond do
      k == :transport_opts ->
        Map.merge(:ranch.normalize_opts(v1), :ranch.normalize_opts(v2), &merger/3)

      Keyword.keyword?(v1) and Keyword.keyword?(v2) ->
        Keyword.merge(v1, v2, &merger/3)

      true ->
        v2
    end
  end
end
