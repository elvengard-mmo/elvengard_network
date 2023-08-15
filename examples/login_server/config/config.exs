import Config

config :login_server, LoginServer.Endpoint,
  listener_name: :login_server,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 3000],
  protocol: LoginServer.Endpoint.Protocol

config :login_server, LoginServer.Endpoint.Protocol,
  packet_handler: LoginServer.Endpoint.PacketHandlers,
  network_codec: LoginServer.Endpoint.NetworkCodec
