import Config

config :logger, :console,
  level: :debug,
  format: "[$time] [$level] $message\n",
  colors: [info: :green]

config :echo_server, :endpoint,
  # adapter: ElvenGard.Network.Endpoint.Ranch,
  # listener_name: :echo_server,
  # transport: :ranch_tcp,
  # transport_opts: [ip: "127.0.0.1", port: 3333],
  ip: "127.0.0.1",
  port: 3333,
  socket_handler: EchoServer.SocketHandler,
  transport: :tcp

# config :echo_server, EchoServer.SocketHandler,
#   # Here, the packet handler is not needed because we bypass the packet
#   # handling by returning `:skip` in `handle_message/2`
#   packet_handler: :unset,
#   # Here we are not using the network encoder/decoder.
#   # We send raw packets
#   network_codec: :unset
