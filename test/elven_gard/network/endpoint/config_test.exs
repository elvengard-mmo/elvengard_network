defmodule ElvenGard.Network.Endpoint.ConfigTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint
  alias ElvenGard.Network.Endpoint.Adapters.Ranch

  @configured_options [
    adapter: Ranch,
    adapter_options: [num_acceptors: 10],
    ip: "127.0.0.1",
    listener_name: :endpoint,
    port: 5555,
    socket_handler: MyApp.SocketHandler,
    transport: :ssl,
    transport_options: [reuseaddr: true]
  ]

  describe "config/2" do
    test "returns generic endpoint configuration" do
      config =
        Endpoint.Config.config(:elvengard_network, MyApp.Endpoint, @configured_options)

      assert Map.new(config) == %{
               otp_app: :elvengard_network,
               adapter: Ranch,
               adapter_options: [num_acceptors: 10],
               ip: "127.0.0.1",
               port: 5555,
               transport: :ssl,
               transport_options: [reuseaddr: true],
               listener_name: :endpoint,
               socket_handler: MyApp.SocketHandler
             }
    end

    test "provides adapter-independent defaults" do
      config =
        Endpoint.Config.config(
          :elvengard_network,
          MyApp.UnconfiguredEndpoint,
          adapter: Ranch,
          socket_handler: MyApp.SocketHandler
        )

      assert Map.new(config) == %{
               otp_app: :elvengard_network,
               adapter: Ranch,
               adapter_options: [],
               listener_name: MyApp.UnconfiguredEndpoint,
               ip: {127, 0, 0, 1},
               port: 3000,
               transport: :tcp,
               transport_options: [],
               socket_handler: MyApp.SocketHandler
             }
    end

    test "requires an endpoint adapter" do
      assert_raise KeyError, fn ->
        Endpoint.Config.config(
          :elvengard_network,
          MyApp.Endpoint,
          socket_handler: MyApp.SocketHandler
        )
      end
    end
  end
end
