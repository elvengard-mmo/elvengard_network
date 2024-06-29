# Protocol

In this section, we will learn how to use `ElvenGard.Network.Endpoint.Protocol`.

An Protocol is wrapper around [Ranch protocols](https://ninenines.eu/docs/en/ranch/2.1/guide/protocols/).

## Configuration

Configuration is performed via the `config/config.exs` file as you did previously.

```elixir
config :login_server, LoginServer.Endpoint.Protocol,
  packet_handler: LoginServer.Endpoint.PacketHandler,
  network_codec: LoginServer.Endpoint.NetworkCodec
```

Here's the explanation for these options:

  - `packet_handler: LoginServer.PacketHandler`: specifies the module responsible for
    handling packets received from clients.
  - `network_codec: LoginServer.NetworkCodec`: Specifies the module responsible for 
    encoding and decoding packets for communication between clients and the server.

## Create a Protocol

For this part, we're going to create a fairly simple protocol that will just display 
our packets.

```elixir
# file: lib/login_server/endpoint/protocol.ex
defmodule LoginServer.Endpoint.Protocol do
  @moduledoc """
  Documentation for LoginServer.Endpoint.Protocol
  """

  use ElvenGard.Network.Endpoint.Protocol

  require Logger

  alias ElvenGard.Network.Socket

  ## Callbacks

  @impl true
  def handle_connection(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")

    %Socket{transport: transport, transport_pid: transport_pid} = socket
    :ok = transport.setopts(transport_pid, packet: :line, reuseaddr: true)

    {:ok, socket}
  end

  @impl true
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message from #{socket.id}: #{inspect(message)}")
    {:skip, socket}
  end

  @impl true
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end
end
```

Once again, creating a Protocol is fairly straightforward.

This example defines 3 callbacks :

  - `handle_connection/1`: called when a client connects, it is mainly used to set 
    [socket options](https://www.erlang.org/doc/man/inet#setopts-2) or 
    call `ElvenGard.Network.Socket.assign/2` to init assigns.
  - `handle_message/2`: called when we receive a packet from a client, we can 
    either skip it by returning `:skip`, or choose to decode it and then 
    handle it by returning `:ok`.
  - `handle_halt/2`: called when a client disconnects.

**NOTE**: you may notice that we define the `packet: :line` option in `handle_connection/1`. 
We use this option because we want to use a line break as a separator for our packets. 
This works because, according to our [network protocol](network_protocol.html), we use 
a text protocol where each packet is separated by a `\n`. However, for a binary protocol, 
you may need to use `packet: :raw` or other options.  
For more information on available options, see `:inet.setopts/2`.

## Summary

If you run your application in its current state, you'll see that it's possible 
to connect to our server and send it messages, and that these are displayed by 
our application.  
Since we're using a text-based protocol, you can use Netcat as client for example.

Now that packets can be received, they need to be decoded and processed. 
This is what we'll see next.
