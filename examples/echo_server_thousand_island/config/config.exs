import Config

config :logger, :console,
  level: :debug,
  format: "[$time] [$level] $message\n",
  colors: [info: :green]

config :echo_server, :endpoint,
  adapter: ElvenGard.Network.Endpoint.ThousandIsland,
  ip: "127.0.0.1",
  port: 3333,
  socket_handler: EchoServer.SocketHandler,
  transport: :tcp

# config :echo_server, EchoServer.SocketHandler,
#   # Here, the packet handler is not needed because we bypass the packet
#   # handling by returning `:skip` in `handle_data/2`
#   packet_handler: :unset,
#   # Here we are not using the network encoder/decoder.
#   # We send raw packets
#   network_codec: :unset
