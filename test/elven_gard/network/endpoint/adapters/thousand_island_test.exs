defmodule ElvenGard.Network.Endpoint.Adapters.ThousandIslandTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint.Adapters.ThousandIsland, as: Adapter
  alias ElvenGard.Network.Endpoint.Protocol
  alias ThousandIsland, as: ThousandIslandServer

  @base_config [
    otp_app: :elvengard_network,
    adapter: Adapter,
    adapter_options: [num_acceptors: 10, read_timeout: 5_000],
    listener_name: :test_thousand_island_listener,
    ip: "127.0.0.1",
    port: 5555,
    socket_handler: __MODULE__.SocketHandler,
    transport: :tcp,
    transport_options: [reuseaddr: true]
  ]

  test "builds a Thousand Island child spec from generic endpoint configuration" do
    assert %{
             id: {ThousandIslandServer, __MODULE__.Endpoint, :test_thousand_island_listener},
             start: {ThousandIslandServer, :start_link, [options]},
             restart: :permanent,
             type: :supervisor
           } = Adapter.child_spec(__MODULE__.Endpoint, @base_config)

    assert options[:handler_module] == Protocol.ThousandIsland

    assert options[:handler_options] == [
             otp_app: :elvengard_network,
             socket_handler: __MODULE__.SocketHandler
           ]

    assert options[:num_acceptors] == 10
    assert options[:read_timeout] == 5_000
    assert options[:port] == 5555
    assert options[:supervisor_options] == [name: :test_thousand_island_listener]
    assert options[:transport_module] == ThousandIsland.Transports.TCP
    assert options[:transport_options] == [reuseaddr: true, ip: {127, 0, 0, 1}]
  end

  test "overrides a server name from adapter options with the endpoint listener name" do
    config =
      Keyword.put(
        @base_config,
        :adapter_options,
        supervisor_options: [name: :wrong_listener]
      )

    %{start: {ThousandIslandServer, :start_link, [options]}} =
      Adapter.child_spec(__MODULE__.Endpoint, config)

    assert options[:supervisor_options] == [name: :test_thousand_island_listener]
  end

  test "selects the Thousand Island SSL transport" do
    config = Keyword.put(@base_config, :transport, :ssl)

    %{start: {ThousandIslandServer, :start_link, [options]}} =
      Adapter.child_spec(__MODULE__.Endpoint, config)

    assert options[:transport_module] == ThousandIsland.Transports.SSL
  end

  test "raises for an invalid IP address" do
    config = Keyword.put(@base_config, :ip, "1.2.3.4.5")

    assert_raise ArgumentError, ~r/invalid IP address/, fn ->
      Adapter.child_spec(__MODULE__.Endpoint, config)
    end
  end
end
