import Config

config :login_server, LoginServer.Endpoint,
  adapter: ElvenGard.Network.Endpoint.Adapters.Ranch,
  adapter_options: [],
  ip: "127.0.0.1",
  listener_name: :login_server,
  port: 3000,
  socket_handler: LoginServer.Endpoint.SocketHandler,
  transport: :tcp,
  transport_options: []

config :login_server, LoginServer.Endpoint.SocketHandler,
  packet_handler: LoginServer.Endpoint.PacketHandler,
  network_codec: LoginServer.Endpoint.NetworkCodec
