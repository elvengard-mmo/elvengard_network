# Todo list

- Abstract the message handler and provide a generic way to handle all transport (ranch, gen_tcp, gen_udp, ...)
- Refacto ElvenGard.Network.PacketSerializer
- Refacto ElvenGard.Network.Endpoint.Protocol
- Document all `use` according to the bests practices
- Add telemetry
- use @derive for packet serialization/deserialization instead of `@serializable` and `@deserializable` ??
- mix task `elven_network.new` to create a project structure
