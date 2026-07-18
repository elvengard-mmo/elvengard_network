defmodule ElvenGard.Network.Endpoint do
  @moduledoc """
  Defines a supervised network listener backed by a configurable endpoint adapter.

  An endpoint owns listener configuration and delegates server-specific operations
  to a module implementing `ElvenGard.Network.Endpoint.Adapter`.

  For in-depth information on how to use and configure network endpoints, please
  refer to the [Endpoint documentation](https://hexdocs.pm/elvengard_network/endpoint.html).
  """

  alias __MODULE__.Config, as: EndpointConfig

  @type transport :: :tcp | :ssl
  @type ip_address :: :inet.ip_address() | String.t()

  @type option ::
          {:adapter, module()}
          | {:adapter_options, Keyword.t()}
          | {:listener_name, atom()}
          | {:ip, ip_address()}
          | {:port, :inet.port_number()}
          | {:socket_handler, module()}
          | {:transport, transport()}
          | {:transport_options, Keyword.t()}

  @type options :: [option()]
  @type config :: [{:otp_app, atom()} | option()]

  @type runtime_options :: [
          socket_handler: module(),
          network_codec: module(),
          packet_handler: module()
        ]

  ## Callbacks

  @doc "Called just before starting the endpoint listener."
  @callback handle_start(config :: config()) :: :ok

  ## Public API

  @doc false
  @spec __using__(Macro.t()) :: Macro.t()
  defmacro __using__(opts) do
    quote location: :keep do
      @behaviour unquote(__MODULE__)

      @otp_app unquote(opts)[:otp_app] || raise("endpoint expects :otp_app to be given")

      @config unquote(EndpointConfig).config(
                @otp_app,
                __MODULE__,
                Application.compile_env(@otp_app, __MODULE__, [])
              )

      unquote(definitions())
      unquote(default_callbacks())
    end
  end

  ## Private function

  defp definitions() do
    quote location: :keep do
      @doc """
      Returns a specification to start this module under a supervisor.

      See `Supervisor`.
      """
      @spec child_spec(Keyword.t()) :: Supervisor.child_spec()
      def child_spec(_opts) do
        runtime_options = unquote(EndpointConfig).runtime_options(@config)
        :ok = handle_start(@config)

        adapter = Keyword.fetch!(@config, :adapter)
        adapter.child_spec(__MODULE__, @config, runtime_options)
      end

      @doc """
      Returns the Endpoint's configuration.
      """
      @spec config() :: ElvenGard.Network.Endpoint.config()
      def config() do
        @config
      end

      @doc """
      Returns the listening address.
      """
      @spec get_addr() :: String.t()
      def get_addr() do
        adapter = Keyword.fetch!(@config, :adapter)
        adapter.get_addr(__MODULE__, @config)
      end

      @doc """
      Returns the listening port.
      """
      @spec get_port() :: :inet.port_number()
      def get_port() do
        adapter = Keyword.fetch!(@config, :adapter)
        adapter.get_port(__MODULE__, @config)
      end
    end
  end

  defp default_callbacks() do
    quote do
      @impl true
      def handle_start(_config), do: :ok

      defoverridable handle_start: 1
    end
  end
end
