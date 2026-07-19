# Socket Handler

In this section, we will learn how to use `ElvenGard.Network.SocketHandler`.

A socket handler defines transport-independent callbacks. The Endpoint runs it
through the selected Ranch or Thousand Island runtime; application modules do
not implement callbacks from either network server.

## Configuration

Configuration is performed via the `config/config.exs` file as you did previously.

```elixir
config :login_server, LoginServer.Endpoint.SocketHandler,
  packet_handler: LoginServer.Endpoint.PacketHandler,
  network_codec: LoginServer.Endpoint.NetworkCodec
```

Here's the explanation for these options:

  - `packet_handler: LoginServer.Endpoint.PacketHandler`: specifies the module
    responsible for handling packets received from clients.
  - `network_codec: LoginServer.Endpoint.NetworkCodec`: specifies the module
    responsible for encoding and decoding packets for communication between
    clients and the server.

These modules are resolved once when the endpoint starts, rather than for every
connection. If `handle_message/2` always skips packet processing, both values may
be set to `:unset`.

## Create a Socket Handler

For this part, we're going to create a fairly simple socket handler that will just display 
our packets.

```elixir
# file: lib/login_server/endpoint/socket_handler.ex
defmodule LoginServer.Endpoint.SocketHandler do
  @moduledoc """
  Documentation for LoginServer.Endpoint.SocketHandler
  """

  use ElvenGard.Network.SocketHandler

  require Logger

  alias ElvenGard.Network.Socket

  ## Callbacks

  @impl ElvenGard.Network.SocketHandler
  def handle_init(%Socket{} = socket) do
    Logger.info("New connection: #{socket.id}")
    :ok = Socket.setopts(socket, packet: :line, reuseaddr: true)

    {:ok, socket}
  end

  @impl ElvenGard.Network.SocketHandler
  def handle_message(message, %Socket{} = socket) do
    Logger.debug("New message from #{socket.id}: #{inspect(message)}")
    :ignore
  end

  @impl ElvenGard.Network.SocketHandler
  def handle_info({:send_packet, packet}, %Socket{} = socket) do
    :ok = Socket.send(socket, packet)
    {:ok, socket}
  end

  @impl ElvenGard.Network.SocketHandler
  def handle_halt(reason, %Socket{} = socket) do
    Logger.info("#{socket.id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end
end
```

Once again, creating a socket handler is fairly straightforward.

This example defines four callbacks:

  - `handle_init/1`: called when a client connects. It returns `{:ok, socket}`,
    `{:ok, socket, timeout}`, or `{:stop, reason, socket}`. It is commonly used
    to set [socket options](https://www.erlang.org/doc/man/inet#setopts-2) and
    initialize assigns with `ElvenGard.Network.Socket.assign/2`.
  - `handle_message/2`: called with the current unconsumed bytes followed by the
    newly received binary. Return `:ignore` or `{:ignore, socket}` to discard
    those bytes without decoding them, `{:ok, socket}` to run the network codec
    and packet handler, or `{:stop, reason, socket}` to close the connection.
  - `handle_info/2`: called when the connection process receives an application
    message. It runs inside the process owning the connection, so packet encoding
    and socket assigns remain serialized with incoming traffic. Return
    `{:ok, socket}` to continue or `{:stop, reason, socket}` to close the
    connection.
  - `handle_halt/2`: called exactly once when the connection halts. Transport
    reasons are normalized to `:closed`, `:timeout`, or `{:error, reason}`;
    application code may supply another reason. It must return `{:ok, socket}`.

**NOTE**: you may notice that we define the `packet: :line` option in `handle_init/1`. 
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
