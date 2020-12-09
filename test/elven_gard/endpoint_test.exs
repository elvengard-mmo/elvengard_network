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
    ]
  ]

  Application.put_env(:elven_gard, __MODULE__.Endpoint, @config)

  defmodule Endpoint do
    use ElvenGard.Endpoint, otp_app: :elven_gard

    # Assert endpoint variables
    assert is_list(config)
    assert @otp_app == :elven_gard
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
                 __MODULE__.Endpoint,
                 []
               ]
             },
             type: :supervisor
           }
  end
end
