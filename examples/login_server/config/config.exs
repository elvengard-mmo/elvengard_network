import Config

config :login_server, LoginServer.Endpoint,
  listener_name: :login_server,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 3000],
  socket_handler: LoginServer.Endpoint.SocketHandler

config :login_server, LoginServer.Endpoint.SocketHandler,
  packet_handler: LoginServer.Endpoint.PacketHandler,
  network_codec: LoginServer.Endpoint.NetworkCodec
