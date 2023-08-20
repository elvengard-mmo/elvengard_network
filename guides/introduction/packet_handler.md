# Packet Handler

If you remember, in our `config.exs`, we defined the following lines:

```elixir
config :login_server, LoginServer.Endpoint.Protocol,
  packet_handler: LoginServer.Endpoint.PacketHandler,
  ...
```

Once the packets have been decoded by our Network Codec, they will be redirected to this module. 
This module must implement the `ElvenGard.Network.PacketHandler` protocol.

A Packet Handler is the module that will manage the logic associated with our client packets. 

Let's see how to create one for our demo application.

## Create a PacketHandler

```elixir
# file: lib/login_server/endpoint/packet_handler.ex
defmodule LoginServer.Endpoint.PacketHandler do
  @moduledoc """
  LoginServer.Endpoint.PacketHandler
  """

  @behaviour ElvenGard.Network.PacketHandler

  alias ElvenGard.Network.Socket

  alias LoginServer.ClientPackets.{PingRequest, LoginRequest}
  alias LoginServer.PacketViews

  ## Handlers

  @impl true
  def handle_packet(%PingRequest{}, socket) do
    render = PacketViews.render(:pong_response, %{time: DateTime.utc_now()})
    :ok = Socket.send(socket, render)
    {:cont, socket}
  end

  def handle_packet(%LoginRequest{username: username, password: password}, socket) do
    render =
      if auth_using_db(username, password) do
        PacketViews.render(:login_succeed, %{world: get_worlds_from_manager()})
      else
        PacketViews.render(:login_failed, %{reason: "Bad credentials :/`"})
      end

    :ok = Socket.send(socket, render)
    {:halt, socket}
  end

  ## Fake functions

  defp auth_using_db(username, password) do
    case {username, password} do
      {"admin", "password"} -> true
      _ -> false
    end
  end

  defp get_worlds_from_manager() do
    %{host: "127.0.0.1", port: 5000}
  end
end
```

Note that each handler takes as parameter a structure with decoded fields representing the packet 
and the socket associated with it. A handler must return `{:cont, new_socket}` if we want to 
continue receiving packets, or `{:halt, new_socket}` to close the connection to the socket 
and shutdown the associated GenServer. 

Here, our handlers are quite simple: the first will just send to our client a `PongResponse` packet 
with the current time and the second will return a `LoginSucceed` or a `LoginFailed` depending 
on the credentials passed in parameter.

Note also that after a `PingRequest`, we'll continue to handle packets, whereas after a 
`LoginRequest` we'll automatically close the connection.

## Summary

In this section, we've learned how to handle client packets, create logic around them and use 
the previously created [Views](packet_views.html).

Our application is now ready for use. You can view the whole source code at
[examples/login_server](https://github.com/ImNotAVirus/elvengard_network/tree/main/examples/login_server).
