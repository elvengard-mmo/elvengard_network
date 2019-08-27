# Getting Started

This guide is an introduction to [ElvenGard](https://github.com/ImNotAVirus/ElvenGard_V2), a MMORPG Game Server toolkit written in Elixir.  
The purpose of this toolkit is to provide a set of modules and processes to simplify the creation of a game server. It's therefore simple to use, flexible and flexible in order to allow you to concentrate on the implementation of your features.

In this guide, we will see the basics of using ElvenGard to create a simple login server that can receive and manage different textual requests.

For more details on other protocols or features, please refer to the other guides

## Adding ElvenGard to an application 

Let's start creating a new Elixir application by running this command:

```
mix new login_server --sup
```

The `--sup` option ensures that this application has [a supervision tree](http://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html), which we'll need for ElvenGard a little later on.

To add ElvenGard to this application, just add an entry to your mix.exs:

```elixir
defp deps do
  [
    {:elven_gard, github: "imnotavirus/elvengard_v2"}
  ]
end
```

To install this dependency, we will run this command:

```
mix deps.get
```

## Creating our first Frontend

**WIP**
