defmodule ElvenGard.Network.Endpoint.Adapters.RanchTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint.Adapters.Ranch
  alias ElvenGard.Network.Endpoint.Protocol

  @base_config [
    otp_app: :elvengard_network,
    adapter: Ranch,
    adapter_options: [num_acceptors: 10],
    listener_name: :test_listener,
    ip: "127.0.0.1",
    port: 5555,
    socket_handler: __MODULE__.SocketHandler,
    transport: :tcp,
    transport_options: [reuseaddr: true]
  ]

  test "builds a Ranch child spec from generic endpoint configuration" do
    assert Ranch.child_spec(__MODULE__.Endpoint, @base_config) == %{
             id: {:ranch_embedded_sup, {__MODULE__.Endpoint, :test_listener}},
             start: {
               :ranch_embedded_sup,
               :start_link,
               [
                 {__MODULE__.Endpoint, :test_listener},
                 :ranch_tcp,
                 %{
                   num_acceptors: 10,
                   socket_opts: [reuseaddr: true, ip: {127, 0, 0, 1}, port: 5555]
                 },
                 Protocol.Ranch,
                 [
                   otp_app: :elvengard_network,
                   socket_handler: __MODULE__.SocketHandler
                 ]
               ]
             },
             type: :supervisor
           }
  end

  test "selects the Ranch SSL transport" do
    config = Keyword.put(@base_config, :transport, :ssl)

    %{start: {:ranch_embedded_sup, :start_link, [_ref, transport | _rest]}} =
      Ranch.child_spec(__MODULE__.Endpoint, config)

    assert transport == :ranch_ssl
  end

  test "raises for an invalid IP address" do
    config = Keyword.put(@base_config, :ip, "1.2.3.4.5")

    assert_raise ArgumentError, ~r/invalid IP address/, fn ->
      Ranch.child_spec(__MODULE__.Endpoint, config)
    end
  end
end
