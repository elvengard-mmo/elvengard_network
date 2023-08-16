# Packet Definitions

In this section, we will learn how to use `ElvenGard.Network.PacketSerializer`.

We'll use the previously created types to transform our 
[network protocol](network_protocol.html) into Elixir structures.

## `defpacket` Macro

First, we'll take a look at how the `ElvenGard.Network.PacketSerializer.defpacket/3`
macro works.

Here is the most basic example for a packet definition that you can have:

```elixir
# Textual packet
defpacket "HEADER", as: TextualPacket

# Binary packet
defpacket 0x0000, as: BinaryPacket
```

This will generate a simple structure, without any field.

The first parameter of the `defpacket` is its ID. It's used to identify it, so that we 
know which fields to decode next.

We also have to specify a `:as` option. This is the name of the generated structure. For 
example, the following code will generate a structure called `MyApp.MyPacket` with the 
binary packet header `0x0000`.

```elixir
defmodule MyApp do
  defpacket 0x0000, as: MyPacket
end
```

### Guards

Now, let's imagine you have several packets with the same packet header and to find out 
how to deserialize it, you need to access a socket-related state. To do this, you can 
use socket assigns and add a guard to the packet:

```elixir
defpacket 0x0000 when socket.assigns.state == :init, as: InitPacket
defpacket 0x0000 when socket.assigns.state == :sync, as: SyncPacket
```

In this example, if when deserializing the packet, the socket state is `:init`, the 
structure returned by our deserializer will be `InitPacket` and if it is `:sync`, it 
will be `SyncPacket`.

For more information and examples of how to use guards, I recommend you read the code in 
[examples/minecraft_ex](https://github.com/ImNotAVirus/elvengard_network/tree/main/examples/minecraft_ex).

**NOTE**: guards are only available for deserialization (client packets).

### Body

Finally, to define fields in our packet, their name, type etc., we'll use a `do ... end` block.

```elixir
defpacket 0x0000, as: MyPacket do
  field ...
end
```

## `field` Macro

- type
- options


## Decorators



## serialize/1, deserialize/3 (both)



## Client Packets

```elixir
# file: lib/login_server/client_packets.ex
defmodule LoginServer.ClientPackets do
  @moduledoc """
  Documentation for LoginServer.ClientPackets
  """

  use ElvenGard.Network.PacketSerializer

  alias LoginServer.Types.StringType

  ## Ping packet

  @deserializable true
  defpacket "PING", as: PingRequest

  ## Login packet

  @deserializable true
  defpacket "LOGIN", as: LoginRequest do
    field :username, StringType
    field :password, StringType
  end
end
```

## Server Packets

```elixir
# file: lib/login_server/server_packets.ex
defmodule LoginServer.ServerPackets do
  @moduledoc """
  Documentation for LoginServer.ServerPackets
  """

  use ElvenGard.Network.PacketSerializer

  alias LoginServer.Types.{DateTimeType, StringType}
  alias LoginServer.SubPackets.WorldInfo

  ## Ping packet

  @serializable true
  defpacket "PONG", as: PongResponse do
    field :time, DateTimeType
  end

  ## Login packets

  @serializable true
  defpacket "FAIL", as: LoginFailed do
    field :reason, StringType
  end

  @serializable true
  defpacket "SUCCESS", as: LoginSucceed do
    field :world, WorldInfo, sep: ":"
  end
end
```
