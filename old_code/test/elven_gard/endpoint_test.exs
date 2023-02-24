Code.require_file("../fixtures/frontend_protocols.exs", __DIR__)

defmodule ElvenGard.EndpointTest do
  use ExUnit.Case, async: true

  # @config [
  #   listener_name: :endpoint_test,
  #   transport: :ranch_tcp,
  #   transport_opts: %{
  #     max_connections: 1024,
  #     num_acceptors: 10,
  #     socket_opts: [
  #       ip: {127, 0, 0, 1},
  #       port: 0,
  #       nodelay: true,
  #       backlog: 1024
  #     ]
  #   },
  #   protocol: __MODULE__.EndpointProtocol,
  #   protocol_opts: [foo: :bar]
  # ]

  @config [
    listener_name: :endpoint_test,
    transport: :ranch_tcp,
    transport_opts: [
      ip: {127, 0, 0, 1},
      port: 0
    ],
    protocol: MyApp.FrontendProtocol
  ]

  Application.put_env(:elven_gard, __MODULE__.Endpoint, @config)

  defmodule Endpoint do
    use ElvenGard.Endpoint, otp_app: :elven_gard

    # Assert endpoint variables
    assert @otp_app == :elven_gard
    assert is_list(config)
  end

  defmodule NoConfigEndpoint do
    use ElvenGard.Endpoint, otp_app: :elven_gard
  end

  setup_all do
    start_supervised!(Endpoint)
    :ok
  end

  test "defines child_spec/1" do
    assert Endpoint.child_spec([]) == %{
             id: {:ranch_embedded_sup, {__MODULE__.Endpoint, :endpoint_test}},
             start: {
               :ranch_embedded_sup,
               :start_link,
               [
                 {__MODULE__.Endpoint, :endpoint_test},
                 :ranch_tcp,
                 %{socket_opts: [ip: {127, 0, 0, 1}, port: 0]},
                 MyApp.FrontendProtocol,
                 []
               ]
             },
             type: :supervisor
           }
  end

  test "warns if there is no configuration for an endpoint" do
    assert ExUnit.CaptureLog.capture_log(fn ->
             NoConfigEndpoint.start_link()
           end) =~ "no configuration"
  end

  test "accepts connections" do
    assert {:ok, _} = connect_to_endpoint()
  end

  test "c:handle_init/1 is called" do
    transport_pid = connect_to_endpoint!()
    assert_receive {:tcp, ^transport_pid, "init done!"}, 500
  end

  test "c:handle_halt/2 is called" do
    transport_pid = connect_to_endpoint!() |> wait_for_init!()
    pid = get_frontend_pid(transport_pid)
    ref = Process.monitor(pid)
    close_connection(transport_pid)

    assert_receive {:tcp_closed, ^transport_pid}, 1000
    assert_receive {:DOWN, ^ref, :process, ^pid, :normal}, 1000
  end

  ## Private functions

  defp connect_to_endpoint() do
    ip = {127, 0, 0, 1}
    port = Endpoint.get_port()
    timeout = 1_000

    :gen_tcp.connect(ip, port, [:binary, active: true], timeout)
  end

  defp connect_to_endpoint!() do
    {:ok, transport_pid} = connect_to_endpoint()
    transport_pid
  end

  defp wait_for_init!(transport_pid) do
    assert_receive {:tcp, ^transport_pid, "init done!"}, 500
    transport_pid
  end

  defp send_data(transport_pid, data) do
    :ok = :gen_tcp.send(transport_pid, data)
  end

  defp get_frontend_pid(transport_pid) do
    send_data(transport_pid, "fpid")
    assert_receive {:tcp, ^transport_pid, "fpid #PID" <> fpid}, 1000
    fpid |> to_charlist() |> :erlang.list_to_pid()
  end

  defp close_connection(transport_pid) do
    :ok = :gen_tcp.shutdown(transport_pid, :read_write)
  end
end
