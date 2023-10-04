# ElvenGard.Network

<!-- MDOC !-->

[![Hex.pm version](https://img.shields.io/hexpm/v/elvengard_network.svg?style=flat)](https://hex.pm/packages/elvengard_network)
[![Hex.pm license](https://img.shields.io/hexpm/l/elvengard_network.svg?style=flat)](https://hex.pm/packages/elvengard_network)
[![Build Status](https://github.com/ImNotAVirus/elvengard_network/actions/workflows/elixir.yml/badge.svg?branch=main)](https://github.com/ImNotAVirus/elvengard_network/actions/workflows/elixir.yml)
[![Coverage Status](https://coveralls.io/repos/github/ImNotAVirus/elvengard_network/badge.svg?branch=main)](https://coveralls.io/github/ImNotAVirus/elvengard_network?branch=main)

## What is ElvenGard

Currently, all independent developers wishing to create a Multiplayer game have already asked themselves the question of how to make the server part easily while being "solid" (minimum crash, latency, ...).  
Indeed, for the client part, there are many tools very well designed to realize it (Unity3D, Unreal Engine,...) but for the server part, each game being different there are currently very few solutions to do this work.

This is the goal of this ambitious project: make a toolkit to group together different functionalities present in any MMORPG (network part, quests, movements, objects in game, instances, etc...) to prepare bases for the developer so that he doesn't have to dwell on this part which is often tedious.

## What is ElvenGard.Network

[ElvenGard.Network](https://github.com/ImNotAVirus/elvengard_network) is a dedicated toolkit designed to streamline and enhance the network aspect of game server development. Built on top of the "ranch" Erlang library, this powerful toolkit provides developers with a robust foundation for creating multiplayer game servers with ease.

Key Features:

1. **Specialized Network Handling:** ElvenGard.Network is solely focused on handling the complexities of networking in multiplayer game servers. It offers a high-performance solution optimized for managing player connections, communication, and data exchange.

2. **Efficient Protocol Management:** The toolkit simplifies the implementation of network protocols, allowing developers to define and manage packet structures efficiently. This streamlined approach ensures smooth and reliable communication between the game client and server.

3. **Packet Processing Made Easy:** ElvenGard.Network streamlines packet processing, making it straightforward to handle incoming and outgoing data. Developers can effortlessly manage packet reception, interpretation, and response, thereby reducing the burden of low-level network management.

4. **Custom Serialization/Deserialization:** With support for custom data types, ElvenGard.Network enables seamless serialization and deserialization of data. This feature ensures compatibility and efficient data exchange between different components of the game server.

5. **Minimized Latency and Crashes:** By leveraging the "ranch" library, ElvenGard.Network benefits from Erlang's reliability and scalability. This helps minimize latency issues and reduces the risk of crashes, providing a stable networking foundation for multiplayer games.

With ElvenGard.Network handling the intricate network tasks, game developers can focus on creating captivating gameplay, dynamic worlds, and engaging player experiences. By leveraging this specialized toolkit, the journey from conceptualizing a multiplayer game to deploying a robust server becomes significantly smoother and more efficient.

## Installation

The package can be installed by adding `elvengard_network` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:elvengard_network, "~> 0.1.1"}
  ]
end
```

The docs can be found at [https://hexdocs.pm/elvengard_network](https://hexdocs.pm/elvengard_network).

**/!\ This toolkit is currently not production ready !**

## Projects using ElvenGard

- [MinecraftEx](https://github.com/ImNotAVirus/elvengard_network/tree/main/examples/minecraft_ex): Located in the `examples` folder of the repository, this is the beginning of a [Minecraft](https://www.minecraft.net) server emulator
- [AvantHeim](https://github.com/ImNotAVirus/AvantHeim): Created by the same developer as ElvenGard, this is a [NosTale](https://gameforge.com/en-US/play/nostale) server emulator

## Contributing

I'm currently developing this project. Any review or PR is welcome.  
Also, feel free to fork the repository and contribute.
