defmodule ElvenGard.Endpoint do
  @moduledoc ~S"""
  TODO: Documentation for ElvenGard.Endpoint
  """

  ## User callbacks

  @type endpoint_type :: :runtime | :supervisor

  @callback init(type :: endpoint_type(), config :: Keyword.t()) :: {:ok, Keyword.t()} | :ignore

  @optional_callbacks init: 2

  ## Public API

  @doc false
  defmacro __using__(opts) do
    quote do
      @behaviour ElvenGard.Endpoint

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
    quote location: :keep do
      @doc false
      def __listener_name__() do
        {__MODULE__, unquote(runtime_config(:runtime))[:listener_name]}
      end

      @doc """
      Returns the child specification to start the endpoint
      under a supervision tree.
      """
      def child_spec(opts) do
        config = unquote(runtime_config(:supervisor))

        :ranch.child_spec(
          __listener_name__(),
          config[:transport],
          config[:transport_opts],
          config[:protocol],
          config[:protocol_opts]
        )
      end

      @doc """
      Starts the endpoint.
      """
      def start_link(_opts \\ []) do
        config = unquote(runtime_config(:supervisor))

        result =
          :ranch.start_listener(
            __listener_name__(),
            config[:transport],
            config[:transport_opts],
            config[:protocol],
            config[:protocol_opts]
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

  defp runtime_config(type) do
    quote location: :keep, unquote: false, bind_quoted: [type: type] do
      ElvenGard.Endpoint.Supervisor.runtime_config(
        type,
        __MODULE__,
        @otp_app,
        unquote(Macro.escape(var!(config)))
      )
    end
  end
end
