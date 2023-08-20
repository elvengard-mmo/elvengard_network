# Getting Started

This guide is an introduction to [ElvenGard.Network](https://github.com/ImNotAVirus/elvengard_network), the Network toolkit for a MMO Game Server toolkit written in Elixir.  
The purpose of this toolkit is to provide a set of modules and processes to simplify the creation of a game server. It's therefore simple to use and flexible in order to allow you to concentrate on the implementation of your features.

In this guide, we're going to learn some basics about ElvenGard.Network. If you want
to see the code from this guide, you can view it at [elvengard_network/examples/login_server on GitHub](https://github.com/ImNotAVirus/elvengard_network/tree/main/examples/login_server).

## Adding ElvenGard.Network to an application

To start off with, we'll generate a new Elixir application by running this command:

```
mix new login_server --sup
```

The `--sup` option ensures that this application has [a supervision tree](http://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html), which we'll need for ElvenGard a little later on.

To add ElvenGard.Network to this application, just add an entry to your mix.exs:

```elixir
defp deps do
  [
    {:elvengard_network, "~> 0.1.0-alpha"}
  ]
end
```

To install this dependency, we will run this command:

```
mix deps.get
```

Then, we need to add a bit of configuration in `config/config.exs`

```elixir
import Config

config :login_server, LoginServer.Endpoint,
  listener_name: :login_server,
  transport: :ranch_tcp,
  transport_opts: [ip: "127.0.0.1", port: 3000],
  protocol: LoginServer.Endpoint.Protocol

config :login_server, LoginServer.Endpoint.Protocol,
  packet_handler: LoginServer.Endpoint.PacketHandler,
  network_codec: LoginServer.Endpoint.NetworkCodec
```

This configuration example simply tells our endpoint to listen on port 3000 of the local address and defines the protocol, network encoder and packet handler to be used.
  
For more details on the configuration of each module, please refer to [Endpoint](endpoint.html#configuration) and [Protocol](protocol.html#configuration) guides and [Ranch documentation](https://ninenines.eu/docs/en/ranch/2.1/guide/).

## Summary

At the end of this section, you must have setup your app to use Elvengard.Network. Now that we have everything installed, let's create our first Endpoint and get up and running.
