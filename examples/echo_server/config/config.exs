import Config

config :logger, :console,
  level: :debug,
  format: "$time $metadata[$level] $message\n",
  metadata: [:application],
  colors: [info: :green]

config :echo_server, EchoServer.Endpoint,
  listener_name: :echo_server,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 3333],
  protocol: EchoServer.Endpoint.Protocol,
  protocol_opts: [serializer: ElvenGard.Socket.DummySerializer]
