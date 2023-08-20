defmodule ElvenGard.Network.EndpointTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  Application.put_env(:elvengard_network, __MODULE__.MyEndpoint,
    listener_name: :my_endpoint,
    transport_opts: [ip: {127, 0, 0, 1}, port: 0]
  )

  defmodule MyEndpoint do
    use ElvenGard.Network.Endpoint, otp_app: :elvengard_network

    @behaviour :ranch_protocol

    @impl ElvenGard.Network.Endpoint
    def handle_start(_config) do
      send(self(), :handle_start)
      :ok
    end

    @impl :ranch_protocol
    def start_link(ref, transport, opts) do
      {:ok, :proc_lib.spawn_link(__MODULE__, :init, [{ref, transport, opts}])}
    end

    def init({ref, transport, _opts}) do
      {:ok, socket} = :ranch.handshake(ref)
      transport.close(socket)
      :ignore
    end
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

  test "warns if no config" do
    fun = fn ->
      Code.eval_quoted(
        quote do
          defmodule EndpointWithNoConfig do
            use ElvenGard.Network.Endpoint, otp_app: :elvengard_network
          end
        end
      )
    end

    assert capture_io(:stderr, fun) =~ "no config found"
  end

  test "starts a tcp listener" do
    host = {127, 0, 0, 1}
    port = MyEndpoint.get_port()

    assert {:ok, socket} = :gen_tcp.connect(host, port, [])

    :gen_tcp.close(socket)
  end

  test "__listener_name__/0 returns listener name from env" do
    assert MyEndpoint.__listener_name__() ==
             {ElvenGard.Network.EndpointTest.MyEndpoint, :my_endpoint}
  end

  test "config/0 returns the config" do
    config = MyEndpoint.config()

    assert config[:otp_app] == :elvengard_network
    assert config[:listener_name] == :my_endpoint
    assert config[:transport] == :ranch_tcp
    assert config[:transport_opts]
    assert config[:protocol] == ElvenGard.Network.EndpointTest.MyEndpoint
    assert config[:protocol_opts]
  end

  test "get_addr/0 returns the endpoint's address" do
    assert MyEndpoint.get_addr() == "127.0.0.1"
  end

  test "get_port/0 returns the endpoint's address" do
    assert is_integer(MyEndpoint.get_port())
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
                   ElvenGard.Network.EndpointTest.MyEndpoint,
                   []
                 ]
               },
               type: :supervisor
             }
    end

    test "call c:handle_start/1" do
      MyEndpoint.child_spec([])
      assert_received :handle_start

      MyEndpoint.child_spec(ignore_init: true)
      refute_receive :handle_start
    end
  end
end
