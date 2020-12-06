# ElvenGard

[![Build Status](https://github.com/ImNotAVirus/elvengard-network/workflows/CI/badge.svg?branch=master)](https://github.com/ImNotAVirus/elvengard-network/actions)
[![Coverage Status](https://coveralls.io/repos/github/ImNotAVirus/elvengard-network/badge.svg?branch=master)](https://coveralls.io/github/ImNotAVirus/elvengard-network)

## What is ElvenGard

Currently, all independent developers wishing to create a MMORPG type game have already asked themselves the question of how to make the server part easily while being "solid" (minimum crash, latency, ...).  
Indeed, for the client part, there are many tools very well designed to realize it (Unity3D, Unreal Engine,...) but for the server part, each game being different there are currently very few solutions to do this work.

This is the goal of this project: make a toolkit to group together different functionalities present in any MMORPG (network part, quests, movements, objects in game, instances, etc...) to prepare bases for the developer so that he doesn't have to dwell on this part which is often tedious.

## Who is this project for ?

Initially, this project is intended for anyone who wants to create a game without having to re-code the server part from scratch.  
It is also intended for people who want to create an emulator for an existing game. For my tests, I use Nostale as an example. So I already have a predefined network protocol and I don't have to code a client. Later, I also plan to test it with World of Warcraft and FlyFF. Since the network protocols of these games are totally different, this will allow me to test the abstraction of the different features of this toolkit.

## Installation

Currently not [available in Hex](https://hex.pm/docs/publish), you can use it like that:

```elixir
def deps do
  [
    {:elven_gard, github: "imnotavirus/elvengard_v2"}
  ]
end
```

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `elven_gard` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:elven_gard, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/elven_gard](https://hexdocs.pm/elven_gard).

**/!\ This application is currently not production ready !**

## Projects using ElvenGard

- [Flügel](https://github.com/ImNotAVirus/Flugel-NostaleEmu): Created by the same developer as ElvenGard, this is a [Nostale](http://nostale.com/) emulator
- [Imanity](https://github.com/ImNotAVirus/Imanity-FlyffEmu): Created by the same developer as ElvenGard, this is a [FlyFF](http://flyff.webzen.com) emulator

## Contributing

Currently developing this project, I will often open pull-requests. Any review is welcome.  
Also, feel free to fork the repository.
