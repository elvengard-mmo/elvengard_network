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
  protocol: MinecraftEx.Endpoint.Protocol

config :minecraft_ex, MinecraftEx.Endpoint.Protocol,
  packet_handler: MinecraftEx.Endpoint.PacketHandler,
  network_codec: MinecraftEx.Endpoint.NetworkCodec
