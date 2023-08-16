# Network Protocol

Before we start coding, we need to define our network protocol, i.e. what our packets 
are going to look like, how they are going to be encoded, decoded and so on.

For this guide, we're going to use a **text-based protocol**, meaning that all commands 
exchanged are text that can be read by a human.  
We'll be using the `String` module for parsing and tools such as Netcat as a client.

## Packets

In this section we'll look at the different packets and interactions between client 
and server.

### Ping

A client can send a `PingRequest`:

| Packet ID | Field Name | Field Type | Notes |
|-----------|------------|------------|-------|
| PING      | no fields  |            |       |

When the server receives this packet, it must respond with a `PongResponse`:

| Packet ID | Field Name | Field Type | Notes                                               |
|-----------|------------|------------|-----------------------------------------------------|
| PONG      | time       | DateTime   | Time at which the server received the ping requests | 

### Login

A client can send a `LoginRequest`:

| Packet ID | Field Name | Field Type | Notes |
|-----------|------------|------------|-------|
| LOGIN     | username   | String     |       |
|           | password   | String     |       |

Depending on whether the credentials are correct or not, the server can respond in 2 ways,
`LoginFailed`:

| Packet ID | Field Name | Field Type | Notes                                 |
|-----------|------------|------------|---------------------------------------|
| FAIL      | reason     | String     | The reason why the user cannot log in |

or `LoginSucceed`:

| Packet ID | Field Name | Field Type | Notes                            |
|-----------|------------|------------|----------------------------------|
| SUCCESS   | world_info | WorldInfo  | Host and port of our game server |

## Network encoding/decoding

Now that we know what our packets will look like, we need to define how we're going to 
exchange them on the network.

As Elvengard.Network is currently based on 
[Ranch](https://ninenines.eu/docs/en/ranch/2.1/guide/), it only supports the **TCP protocol**.  
And, as we use a text protocol, we'll just separate our different packets by 
**line breaks** (`\n`).

Here's an example of what each packet might look like once encoded:

  - PingRequest: `PING\n`
  - PongResponse: `PONG 2023-08-15 20:45:07.068297Z\n`
  - LoginRequest: `LOGIN admin password\n`
  - LoginFailed: `FAIL Bad credentials\n`
  - LoginSucceed: `SUCCESS 127.0.0.1:5000\n`

## Summary

Now that you know how your client and server will communicate, you can create and 
configure your project.  
That's what we'll look at in the next section.
