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
  socket_handler: EchoServer.Endpoint.SocketHandler

config :echo_server, EchoServer.Endpoint.SocketHandler,
  # Here, the packet handler is not needed because we bypass the packet
  # handling by returning `ignore` in `handle_message/2`
  packet_handler: :unset,
  # Here we are not using the network encoder/decoder.
  # We send raw packets
  network_codec: :unset
