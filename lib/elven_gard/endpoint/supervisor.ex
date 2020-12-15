defmodule ElvenGard.Endpoint.Supervisor do
  @moduledoc false

  require Logger

  alias ElvenGard.Endpoint.Config

  ## Public API

  @doc """
  Retrieves the runtime configuration.
  """
  @spec runtime_config(:runtime | :supervisor, module(), atom(), Keyword.t()) :: Keyword.t()
  def runtime_config(type, endpoint, otp_app, defaults) do
    if is_nil(Application.get_env(otp_app, endpoint)) do
      Logger.warn(
        "no configuration found for otp_app #{inspect(otp_app)} " <>
          "and module #{inspect(endpoint)}"
      )
    end

    case endpoint_init(type, endpoint, defaults) do
      {:ok, config} -> Config.merge(defaults, config)
      :ignore -> defaults
    end
  end

  ## Private functions

  @doc false
  defp endpoint_init(type, endpoint, config) do
    if Code.ensure_loaded?(endpoint) and function_exported?(endpoint, :init, 2) do
      endpoint.init(type, config)
    else
      {:ok, config}
    end
  end
end
