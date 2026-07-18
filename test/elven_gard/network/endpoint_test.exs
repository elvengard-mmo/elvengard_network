defmodule ElvenGard.Network.EndpointTest do
  use ExUnit.Case, async: true

  Application.put_env(:elvengard_network, __MODULE__.MyEndpoint,
    adapter: ElvenGard.Network.Endpoint.Adapters.Ranch,
    adapter_options: [],
    ip: {127, 0, 0, 1},
    listener_name: :my_endpoint,
    port: 0,
    socket_handler: __MODULE__.MyHandler,
    transport: :tcp,
    transport_options: []
  )

  Application.put_env(:elvengard_network, __MODULE__.MyHandler,
    network_codec: ElvenGard.Network.DummyEncoder,
    packet_handler: __MODULE__.MyPacketHandler
  )

  Application.put_env(:elvengard_network, __MODULE__.DelegatingEndpoint,
    adapter: __MODULE__.DelegatingAdapter,
    socket_handler: __MODULE__.MyHandler
  )

  defmodule MyEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network

    @impl ElvenGard.Network.Endpoint
    def handle_start(_config) do
      send(self(), :handle_start)
      :ok
    end
  end

  defmodule MyHandler do
    use ElvenGard.Network.SocketHandler
  end

  defmodule MyPacketHandler do
    @behaviour ElvenGard.Network.PacketHandler

    @impl true
    def handle_packet(_packet, socket), do: {:cont, socket}
  end

  defmodule DelegatingAdapter do
    @behaviour ElvenGard.Network.Endpoint.Adapter

    ## Callbacks

    @impl true
    def child_spec(endpoint, config) do
      send(self(), {:adapter_child_spec, endpoint, config})
      Supervisor.child_spec({Task, fn -> :ok end}, id: endpoint)
    end

    @impl true
    def get_addr(endpoint, config) do
      send(self(), {:adapter_get_addr, endpoint, config})
      "adapter-address"
    end

    @impl true
    def get_port(endpoint, config) do
      send(self(), {:adapter_get_port, endpoint, config})
      12_345
    end
  end

  defmodule DelegatingEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
  end

  setup_all do
    start_supervised!(MyEndpoint)
    :ok
  end

  test "raises if no opt_app provided" do
    assert_raise RuntimeError, "endpoint expects :otp_app to be given", fn ->
      defmodule EndpointWithNoOtpApp do
        use ElvenGard.Network.Endpoint
      end
    end
  end

  test "raises if no socket handler is configured" do
    assert_raise KeyError, fn ->
      Code.eval_quoted(
        quote do
          defmodule EndpointWithNoConfig do
            use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
          end
        end
      )
    end
  end

  test "starts a tcp listener" do
    host = {127, 0, 0, 1}
    port = MyEndpoint.get_port()

    assert {:ok, socket} = :gen_tcp.connect(host, port, [])

    :gen_tcp.close(socket)
  end

  test "config/0 returns the config" do
    config = MyEndpoint.config()

    assert config[:otp_app] == :elvengard_network
    assert config[:adapter] == ElvenGard.Network.Endpoint.Adapters.Ranch
    assert config[:adapter_options] == []
    assert config[:listener_name] == :my_endpoint
    assert config[:ip] == {127, 0, 0, 1}
    assert config[:port] == 0
    assert config[:transport] == :tcp
    assert config[:transport_options] == []
    assert config[:socket_handler] == ElvenGard.Network.EndpointTest.MyHandler

    assert Enum.sort(Keyword.keys(config)) ==
             [
               :adapter,
               :adapter_options,
               :ip,
               :listener_name,
               :otp_app,
               :port,
               :socket_handler,
               :transport,
               :transport_options
             ]
  end

  test "get_addr/0 returns the endpoint's address" do
    assert MyEndpoint.get_addr() == "127.0.0.1"
  end

  test "get_port/0 returns the endpoint's port" do
    assert is_integer(MyEndpoint.get_port())
  end

  test "delegates listener operations to the configured adapter" do
    config = DelegatingEndpoint.config()

    assert %{id: DelegatingEndpoint} = DelegatingEndpoint.child_spec([])
    assert_received {:adapter_child_spec, DelegatingEndpoint, ^config}

    assert DelegatingEndpoint.get_addr() == "adapter-address"
    assert_received {:adapter_get_addr, DelegatingEndpoint, ^config}

    assert DelegatingEndpoint.get_port() == 12_345
    assert_received {:adapter_get_port, DelegatingEndpoint, ^config}
  end

  describe "child_spec/1" do
    test "generates child spec" do
      assert MyEndpoint.child_spec([]) == %{
               id:
                 {:ranch_embedded_sup, {ElvenGard.Network.EndpointTest.MyEndpoint, :my_endpoint}},
               start: {
                 :ranch_embedded_sup,
                 :start_link,
                 [
                   {ElvenGard.Network.EndpointTest.MyEndpoint, :my_endpoint},
                   :ranch_tcp,
                   %{socket_opts: [ip: {127, 0, 0, 1}, port: 0]},
                   ElvenGard.Network.Endpoint.Protocol.Ranch,
                   [
                     otp_app: :elvengard_network,
                     socket_handler: ElvenGard.Network.EndpointTest.MyHandler
                   ]
                 ]
               },
               type: :supervisor
             }
    end

    test "call c:handle_start/1" do
      MyEndpoint.child_spec([])
      assert_received :handle_start
    end
  end
end
