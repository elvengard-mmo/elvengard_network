defmodule ElvenGard.Endpoint do
  @moduledoc ~S"""
  TODO: Documentation for ElvenGard.Endpoint
  """

  @doc false
  defmacro __using__(opts) do
    quote do
      require Logger

      unquote(config(opts))
      unquote(listener())
    end
  end

  ## Private functions

  defp config(opts) do
    quote do
      @otp_app unquote(opts)[:otp_app] || raise("endpoint expects :otp_app to be given")
      var!(config) = ElvenGard.Endpoint.Config.config(@otp_app, __MODULE__)
    end
  end

  defp listener() do
    quote location: :keep, unquote: false do
      config = Macro.escape(var!(config))

      @doc false
      # TODO: Maybe use ETS later for hot reload support
      def __listener_name__(), do: {__MODULE__, unquote(config)[:listener_name]}

      @doc """
      Returns the child specification to start the endpoint
      under a supervision tree.
      """
      def child_spec(opts) do
        # FIXME: Code duplication between `child_spec/1` and `start_link/1`
        if is_nil(Application.get_env(@otp_app, __MODULE__)) do
          Logger.warn(
            "no configuration found for otp_app #{inspect(@otp_app)} " <>
              "and module #{inspect(__MODULE__)}"
          )
        end

        :ranch.child_spec(
          __listener_name__(),
          unquote(config)[:transport],
          unquote(config)[:transport_opts],
          unquote(config)[:protocol],
          unquote(config)[:protocol_opts]
        )
      end

      @doc """
      Starts the endpoint.
      """
      def start_link(_opts \\ []) do
        # FIXME: Code duplication between `child_spec/1` and `start_link/1`
        if is_nil(Application.get_env(@otp_app, __MODULE__)) do
          Logger.warn(
            "no configuration found for otp_app #{inspect(@otp_app)} " <>
              "and module #{inspect(__MODULE__)}"
          )
        end

        result =
          :ranch.start_listener(
            __listener_name__(),
            unquote(config)[:transport],
            unquote(config)[:transport_opts],
            unquote(config)[:protocol],
            unquote(config)[:protocol_opts]
          )

        # FIXME: Not sure if it's the better way to do this
        if {:ok, pid} = result, do: Process.link(pid)

        result
      end

      @doc """
      Returns the listening address.
      """
      def get_addr() do
        :ranch.get_addr(__listener_name__())
      end

      @doc """
      Returns the listening port.
      """
      def get_port() do
        :ranch.get_port(__listener_name__())
      end
    end
  end
end
