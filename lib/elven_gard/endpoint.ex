defmodule ElvenGard.Endpoint do
  @moduledoc ~S"""
  TODO: Documentation for ElvenGard.Endpoint
  """

  @doc false
  defmacro __using__(opts) do
    quote do
      # @behaviour unquote(__MODULE__)
      use GenServer

      unquote(config(opts))
      unquote(server())
    end
  end

  ## Private functions

  defp config(opts) do
    quote do
      @otp_app unquote(opts)[:otp_app] || raise("endpoint expects :otp_app to be given")
      var!(config) = ElvenGard.Endpoint.Config.config(@otp_app, __MODULE__)
    end
  end

  defp server() do
    quote location: :keep, unquote: false do
      config = Macro.escape(var!(config))

      @doc """
      Returns the child specification to start the endpoint
      under a supervision tree.
      """
      def child_spec(opts) do
        # %{
        #   id: :ranch_sup,
        #   start: {:ranch_sup, :start_link, [opts]},
        #   restart: :permanent,
        #   shutdown: 15_000,
        #   type: :supervisor
        # }
        :ranch.child_spec(
          {__MODULE__, unquote(config)[:listener_name]},
          unquote(config)[:transport],
          unquote(config)[:transport_opts],
          unquote(config)[:protocol],
          unquote(config)[:protocol_opts]
        )
      end

      @doc """
      Starts the endpoint.
      """

      # def start_link(_opts \\ []) do
      #   # :ranch.start_listener(
      #   #   unquote(config)[:listener_name],
      #   #   unquote(config)[:transport],
      #   #   unquote(config)[:transport_opts],
      #   #   unquote(config)[:protocol],
      #   #   unquote(config)[:protocol_opts]
      #   # )
      # end

      @impl true
      def init(init_arg) do
        {:ok, init_arg}
      end
    end
  end
end
