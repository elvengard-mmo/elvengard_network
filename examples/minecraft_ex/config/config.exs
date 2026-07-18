import Config

config :logger, :console,
  level: :debug,
  format: "$time $metadata[$level] $message\n",
  metadata: [:application, :socket_id],
  colors: [info: :green]

config :minecraft_ex, MinecraftEx.Endpoint,
  listener_name: :minecraft_ex,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 25565],
  socket_handler: MinecraftEx.Endpoint.SocketHandler

config :minecraft_ex, MinecraftEx.Endpoint.SocketHandler,
  packet_handler: MinecraftEx.Endpoint.PacketHandler,
  network_codec: MinecraftEx.Endpoint.NetworkCodec
