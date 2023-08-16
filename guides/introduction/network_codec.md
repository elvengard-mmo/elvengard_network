# Network Codec

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
    
    # This will be serialized into `SUCCESS 127.0.0.1:5000\n`
  end
end
```
