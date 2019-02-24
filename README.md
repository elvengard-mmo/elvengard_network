# ElvenGard

[![Build Status](https://travis-ci.com/ImNotAVirus/ElvenGard_V2.svg?branch=master)](https://travis-ci.com/ImNotAVirus/ElvenGard_V2)

## What is ElvenGard

Currently, all independent developers wishing to create a MMORPG type game have already asked themselves the question of how to make the server part easily while being "solid" (minimum crash, latency, ...).  
Indeed, for the client part, there are many tools very well designed to realize it (Unity3D, Unreal Engine,...) but for the server part, each game being different there are currently very few solutions to do this work.

This is the goal of this project: make a toolkit to group together different functionalities present in any MMORPG (network part, quests, movements, objects in game, instances, etc...) to prepare bases for the developer so that he doesn't have to dwell on this part which is often tedious.

## Who is this project for ?

Initially, this project is intended for anyone who wants to create a game without having to re-code the server part from scratch.  
It is also intended for people who want to create an emulator for an existing game. For my tests, I use Nostale as an example. So I already have a predefined network protocol and I don't have to code a client. Later, I also plan to test it with World of Warcraft and FlyFF. Since the network protocols of these games are totally different, this will allow me to test the abstraction of the different features of this toolkit.

## Contributing

Currently developing this project, I will often open pull-requests. Any review is welcome.
