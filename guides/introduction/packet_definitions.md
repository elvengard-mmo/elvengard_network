# Packet Definitions

In this section, we will learn how to use `ElvenGard.Network.PacketSerializer`.

We'll use the previously created types to transform our 
[network protocol](network_protocol.html) into Elixir structures.

## Macros

### `defpacket` Macro

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

#### Guards

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

#### Body

Finally, to define fields in our packet, their name, type etc., we'll use a `do ... end` block.

```elixir
defpacket 0x0000, as: MyPacket do
  field ...
end
```

### `field` Macro

Now that you know how to create a packet, it's time to see how to create fields. To do this, 
you can use the `field` macro.

It's very easy to use:

```elixir
field :field1, FieldType
field :field2, FieldType, opt: :value
```

Here, the first line will add a field to our packet with `field1` as name and `FieldType` 
as type. The name must be an atom and the field type a module which use the 
`ElvenGard.Network.Type` behaviour. The third parameter is optional and is a keyword list
defining options to send when types will be decoded (see `c:ElvenGard.Network.Type.encode/2`
and `c:ElvenGard.Network.Type.decode/2`).

### Decorators

Using `defpacket` and `field`, you can now create structures with different fields, but 
you still can't serialize or deserialize them in order to send them to or receive them 
from the client.

ElvenGard.Network defined 2 decorators for this:

  - `@serializable true`: indicates that the packet is intended to be serialized. It 
    is therefore a **server packet**.
  - `@deserializable true`: indicates that the packet is intended to be deserialized. 
    It is therefore a **client packet**.

By tagging a packet as `deserializable`, the `defpacket` macro will automatically create 
a `deserialize/1` function inside the generated module.

Tagging a packet as `deserializable/3` will, this time, create 2 `deserialize/3` functions, 
one inside the generated module and one outside it. The latter will redirect to the former.

These helper functions will come in very handy in our [Network Codec](network_codec.html), 
as we'll see in the next chapter.

Now that we know the theory, it's time to put it into practice.

## Client Packets

Let's first define our client packets.

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

These packets are pretty straightforward, so there's not much to explain. Just note the 
presence of `@deserializable true`, which clearly indicates that these packets are 
intended to be deserialized as they are received from the client.

## Server Packets

Now let's define the server's one.

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

Once again, these packets are pretty straightforward. We use `@serializable true` to 
indicate that we will send them to the client.

Moreover, we're using the `WorldInfo` sub-packet which, remember, expects the field 
separator as an option. This is where we define it.

## Summary

You now know how to create a packet that can be serialized and deserialized. Now it's 
time to encode or decode it so that it can transit over the network.
