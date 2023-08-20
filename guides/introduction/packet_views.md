# Packet Views

In this section, we will learn how to use `ElvenGard.Network.View`.

The role of a View is to prepare data to be sent via a packet structure.

For example, we may have a packet taking a string as a field, but our backend returns a map. 
Encoding our map every time we need to create a packet would be problematic, as it would 
create code duplication and would be a potential source of errors.

Other common uses are, for example, when a packet has one or more sub-packets, or when we 
need to set default values for certain fields. For simplicity's sake, and to avoid having to 
duplicate this logic in every function that needs to render packets, views have been created.

## Create a Packet Views

```elixir
# file: lib/login_server/packet_views.ex
defmodule LoginServer.PacketViews do
  @moduledoc """
  Documentation for LoginServer.PacketViews
  """

  use ElvenGard.Network.View

  alias LoginServer.ServerPackets.{PongResponse, LoginFailed, LoginSucceed}
  alias LoginServer.SubPackets.WorldInfo

  @impl true
  def render(:pong_response, %{time: time}) do
    %PongResponse{time: time}
  end

  def render(:login_failed, %{reason: reason}) do
    %LoginFailed{reason: reason}
  end

  def render(:login_succeed, %{world: world}) do
    %LoginSucceed{world: %WorldInfo{host: world.host, port: world.port}}
  end
end
```

Not much to explain, except that the `render/2` callback takes as its first parameter an 
identifier (which must be a `String` or an `Atom`) and as its second parameter attributes 
enabling us to generate our packet. Most of the time, the callback returns a strucure 
(a packet), but in exceptional cases it can also directly return iodata. 

## Summary

You now know how to create Views: functions for creating server packets. In the next 
chapter, we'll use this module to create and send packets to our client.
