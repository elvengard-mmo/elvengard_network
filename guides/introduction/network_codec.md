# Network Codec

In this section, we will learn how to use `ElvenGard.Network.NetWorkCodec`.

A NetworkCodec is a behaviour that define how a packet should transit over the network. 
It must define 3 callbacks : 

  - `next/2`: this callback takes a raw binary and a socket as parameters. It returns 
    the first packet found in the form of a tuple: `{packet_binary, rest_of_binary}`.
  - `decode/2`: this callback takes the binary return by `next/2` and returns the 
    deserialized packet (a structure). You should use the `deserialize/3` helper created 
    by [defpacket](packet_definitions.html#decorators)
  - `encode/2`: this callback will be called when you call `ElvenGard.Network.Socket.send/2` 
    with your module as encoder. It returns an 
    [iodata](https://hexdocs.pm/elixir/IO.html#module-io-data) that will, then, be send to 
    your client. You should use the `serialize/1` helper created by 
    [defpacket](packet_definitions.html#decorators)

## Create a Network Codec


```elixir
# file: lib/login_server/endpoint/network_codec.ex
defmodule LoginServer.Endpoint.NetworkCodec do
  @moduledoc """
  Documentation for LoginServer.Endpoint.NetworkCodec
  """

  @behaviour ElvenGard.Network.NetworkCodec

  alias LoginServer.ClientPackets

  ## Behaviour impls

  @impl true
  def next(raw, _socket) do
    case String.split(raw, "\n", parts: 2) do
      [packet] -> {packet, ""}
      [packet, rest] -> {packet, rest}
    end
  end

  @impl true
  def decode(raw, socket) do
    case String.split(raw, " ", parts: 2) do
      [packet_id] -> ClientPackets.deserialize(packet_id, "", socket)
      [packet_id, params] -> ClientPackets.deserialize(packet_id, params, socket)
    end
  end

  @impl true
  def encode(struct, _socket) when is_struct(struct) do
    # %LoginSucceed{world_info: %WorldInfo{host: "127.0.0.1", port: 5000}}
    struct
    # {"SUCCESS", ["127.0.0.1", ":", "5000"]}
    |> struct.__struct__.serialize()
    # ["SUCCESS", ["127.0.0.1", ":", "5000"]]
    |> Tuple.to_list()
    # ["SUCCESS", " ", ["127.0.0.1", ":", "5000"]]
    |> Enum.intersperse(" ")
    # [["SUCCESS", " ", ["127.0.0.1", ":", "5000"]], "\n"]
    |> then(&[&1 | "\n"])
    
    # This iolist will be send as `SUCCESS 127.0.0.1:5000\n`
  end
end
```

Here, our `decode/2` callback uses `LoginServer.ClientPackets.deserialize/3`, which 
redirects to `LoginServer.ClientPackets.<YOUR_PACKET_STRUCTURE>.deserialize/3`.  
Note also that `encode/2` uses `LoginServer.ServerPackets.<YOUR_PACKET_STRUCTURE>.serialize/1` 
to serialize our packet. Finally, we add a `\n` at the end of our packet as defined by 
the [network protocol](network_protocol.html).

## Summary

Now that we've defined our functions for transforming data structures, it's time to see how 
to link them to our [Protocol](protocol.html).
