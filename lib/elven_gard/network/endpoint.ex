defmodule ElvenGard.Network.Endpoint do
  @moduledoc ~S"""
  TODO: Documentation for ElvenGard.Network.Endpoint
  """

  ## Public API

  @doc false
  defmacro __using__(opts) do
    quote do
      @otp_app unquote(opts)[:otp_app] || raise("endpoint expects :otp_app to be given")

      # FIXME: Idk why this doesn't work with Elixir 1.10-12
      Application.compile_env(@otp_app, __MODULE__) ||
        IO.warn("no config found for #{inspect(__MODULE__)}")

      @config ElvenGard.Network.Endpoint.Config.config(@otp_app, __MODULE__)

      unquote(listener())
    end
  end

  ## Private functions

  defp listener() do
    quote do
      @doc false
      def __listener_name__() do
        {__MODULE__, @config[:listener_name]}
      end

      @doc """
      Returns the child specification to start the endpoint
      under a supervision tree.
      """
      def child_spec(opts) do
        :ranch.child_spec(
          __listener_name__(),
          @config[:transport],
          @config[:transport_opts],
          @config[:protocol],
          @config[:protocol_opts]
        )
      end

      @doc """
      Starts the endpoint.
      """
      def start_link(_opts \\ []) do
        :ranch.start_listener(
          __listener_name__(),
          @config[:transport],
          @config[:transport_opts],
          @config[:protocol],
          @config[:protocol_opts]
        )
      end

      @doc """
      Returns the Endpoint's configuration.
      """
      def config() do
        @config
      end

      @doc """
      Returns the listening address.
      """
      def get_addr() do
        __listener_name__() |> :ranch.get_addr() |> elem(0) |> :inet.ntoa() |> List.to_string()
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
