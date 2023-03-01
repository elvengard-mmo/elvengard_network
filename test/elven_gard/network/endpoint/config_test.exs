defmodule ElvenGard.Network.Endpoint.ConfigTest do
  use ExUnit.Case, async: true

  alias ElvenGard.Network.Endpoint

  Application.put_env(:elvengard_network, MyApp.Endpoint1,
    listener_name: :endpoint,
    transport: :ranch_tcp,
    transport_opts: %{socket_opts: [ip: {127, 0, 0, 1}, port: 5555]}
  )

  Application.put_env(:elvengard_network, MyApp.Endpoint2,
    listener_name: :endpoint,
    transport: :ranch_tcp,
    transport_opts: [ip: "127.0.0.1", port: 6666]
  )

  describe "config/2" do
    test "returns valid configs" do
      config = Endpoint.Config.config(:elvengard_network, MyApp.Endpoint1)

      assert config[:otp_app] == :elvengard_network
      assert config[:listener_name] == :endpoint
      assert config[:transport] == :ranch_tcp
      assert is_map(config[:transport_opts])
      assert is_list(config[:transport_opts][:socket_opts])
      assert config[:transport_opts][:socket_opts][:ip] == {127, 0, 0, 1}
      assert config[:transport_opts][:socket_opts][:port] == 5555
    end

    test "can normalize Ranch options" do
      config = Endpoint.Config.config(:elvengard_network, MyApp.Endpoint2)

      assert is_map(config[:transport_opts])
      assert is_list(config[:transport_opts][:socket_opts])
      assert config[:transport_opts][:socket_opts][:ip] == {127, 0, 0, 1}
      assert config[:transport_opts][:socket_opts][:port] == 6666
    end
  end

  describe "from_env/3" do
    test "reads configuration from env" do
      config = Endpoint.Config.from_env(:elvengard_network, MyApp.Endpoint2, [])

      assert config[:listener_name] == :endpoint
      assert config[:transport] == :ranch_tcp
      assert is_list(config[:transport_opts])
      assert config[:transport_opts][:ip] == "127.0.0.1"
      assert config[:transport_opts][:port] == 6666
    end

    test "merge with default configs" do
      config = Endpoint.Config.from_env(:elvengard_network, MyApp.Endpoint2, static: true)
      assert config[:static] == true
    end
  end
end
