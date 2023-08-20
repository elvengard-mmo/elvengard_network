# Endpoint

The first thing to do is define a module using `ElvenGard.Network.Endpoint`.

An Endpoint is simply a [Ranch listener](https://ninenines.eu/docs/en/ranch/2.1/guide/listeners/).

## Configuration

Configuration is performed via the `config/config.exs` file as you did previously.

```elixir
config :login_server, LoginServer.Endpoint,
  listener_name: :login_server,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 3000],
  protocol: LoginServer.Endpoint.Protocol
```

Here's what each configuration option does:

  - `listener_name: :login_server`: specifies a unique name for the listener. This is used by Ranch 
    to manage the listener process.
  - `transport: :ranch_tcp`: specifies the transport protocol to use. In this case, it's Ranch's TCP 
    transport.
  - `transport_opts: [ip: "127.0.0.1", port: 3000]`: provides options for configuring the transport. 
    In this case, it specifies the IP address and port on which the server will listen for incoming 
    connections.
  - `protocol: LoginServer.Endpoint.Protocol`: sets the protocol module that will handle client 
    connections and communication.

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
    host = get_in(config, [:transport_opts, :socket_opts, :ip])
    port = get_in(config, [:transport_opts, :socket_opts, :port])
    Logger.info("LoginServer started on #{:inet.ntoa(host)}:#{port}")
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
Now it's time to create a Protocol to receive our first packets.
