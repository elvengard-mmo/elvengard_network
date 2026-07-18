# Endpoint

The first thing to do is define a module using `ElvenGard.Network.Endpoint`.

An Endpoint is a supervised network listener. Its adapter translates generic
Endpoint configuration to the selected networking server.

## Configuration

Configuration is performed via the `config/config.exs` file as you did previously.

```elixir
config :login_server, LoginServer.Endpoint,
  adapter: ElvenGard.Network.Endpoint.Adapters.Ranch,
  adapter_options: [],
  ip: "127.0.0.1",
  listener_name: :login_server,
  port: 3000,
  socket_handler: LoginServer.Endpoint.SocketHandler,
  transport: :tcp,
  transport_options: []
```

Here's what each configuration option does:

  - `adapter`: selects the module responsible for the listener lifecycle.
  - `adapter_options`: provides server-specific options to the selected adapter.
  - `ip`: specifies the local address on which the listener accepts connections.
  - `listener_name`: specifies a unique name for the listener.
  - `port`: specifies the local port on which the listener accepts connections.
  - `socket_handler: LoginServer.Endpoint.SocketHandler`: sets the socket handler that will handle client
    connections and communication.
  - `transport`: selects `:tcp` or `:ssl` independently from the networking server.
  - `transport_options`: provides options for the underlying socket transport.

## Creating an Endpoint

Here's a basic example you can use for all your projects:

```elixir
# file: lib/login_server/endpoint.ex
defmodule LoginServer.Endpoint do
  @moduledoc """
  Documentation for LoginServer.Endpoint
  """

  use ElvenGard.Network.Endpoint, otp_app: :login_server

  require Logger

  ## Callbacks

  @impl true
  def handle_start(config) do
    host = Keyword.fetch!(config, :ip)
    port = Keyword.fetch!(config, :port)
    Logger.info("LoginServer started on #{host}:#{port}")
  end
end
```

As you can see, creating an endpoint is very simple, you just need to specify the 
otp app you used in the config and you're done.

It is also possible to define the `c:ElvenGard.Network.Endpoint.handle_start/1` 
callback. This allows you to, for example, display various information relating 
to the startup. It receives the endpoint configs as a parameter and must always 
return `:ok`.

## Add to supervision tree

Once you've created and configured your endpoint, all you have to do is place it 
in your application's supervision tree.

```elixir
# file: lib/login_server/application.ex

  ...

  def start(_type, _args) do
    children = [
      LoginServer.Endpoint
    ]

  ...
```

## Summary

At the end of this part, you should have a working Endpoint listening on port 3000 
(see config file).  
Now it's time to create a socket handler to receive our first packets.
