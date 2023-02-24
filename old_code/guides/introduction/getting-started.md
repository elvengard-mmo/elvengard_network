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

As mentioned in the [Frontend guide](frontends.html), the Frontend is the module used to manage interaction with clients.  
The one we will create here will be simple and will not cover all features of the module. For more information, please consult the above-mentioned guide.

NOTE: I'll later add a mix task (`mix elven.gen.frontend`) in order to automatically initialize a project.

First of all, let's create a new file at `lib/login_server/frontend.ex` :

```elixir
defmodule LoginServer.Frontend do
  @moduledoc false

  use ElvenGard.Frontend,
    packet_protocol: LoginServer.PacketProtocol,
    packet_handler: LoginServer.PacketHandler

  alias ElvenGard.Socket

  require Logger

  @impl ElvenGard.Frontend
  def handle_init(args) do
    ip = Keyword.fetch!(args.listener_opts.socket_opts, :ip)
    port = Keyword.fetch!(args.listener_opts.socket_opts, :port)
    Logger.info("Listening for connections on #{:inet.ntoa(ip)}:#{port}")
    {:ok, nil}
  end

  @impl ElvenGard.Frontend
  def handle_connection(socket) do
    Logger.info("New connection: #{socket.id}")
    {:ok, socket}
  end

  @impl ElvenGard.Frontend
  def handle_disconnection(%Socket{id: id} = socket, reason) do
    Logger.info("#{id} is now disconnected (reason: #{inspect(reason)})")
    {:ok, socket}
  end

  @impl ElvenGard.Frontend
  def handle_message(%Socket{id: id} = socket, message) do
    Logger.info("New message from #{id} (len: #{byte_size(message)})")
    {:ok, socket}
  end
end
```

As you can see, our Frontend depends on two other modules:

- [PacketProtocol](protocols.html): the Protocol is the module that will describe the serialization and deserialization of all packets that travel between the client and our server
- [PacketHandler](packet_handlers.html): the packet handler will allow us to define the structure of packets that we will receive

Don't forget to setup the `LoginServer.Frontend` as a process within the application's supervision tree, which we can do in `lib/login_server/application.ex`, inside the `start/2` function:

```elixir
def start(_type, _args) do
  children = [
    LoginServer.Frontend
  ]

  ...
```

## Creating the Protocol

As we said in the introduction of this guide, we will use a text protocol for this example.  
Our packets will therefore be defined in the following format:

```
packet_header [parameter1] [parameter2] ...
```

So, let's create a new file at `lib/login_server/packet_protocol.ex` :

```elixir
defmodule LoginServer.PacketProtocol do
  @moduledoc false

  use ElvenGard.Protocol.Textual,
    model: LoginServer.PacketHandler,
    separator: " "

  # Just add a line feed before sending our packets to clients
  @impl ElvenGard.Protocol
  def encode(data), do: data <> "\n"

  @impl ElvenGard.Protocol
  def decode(data) do
    res =
      data
      |> String.trim()  # Removes the trailing line feed
      |> String.split(" ", parts: 2) # Detaches the packet header from parameters

    # `decode/1` must return a tuple like `{packet_header, params}`
    case res do
      [header] -> {header, ""}
      _ -> List.to_tuple(res)
    end
  end
end
```

Since `ElvenGard.Protocol.Textual` is a wrapper of `ElvenGard.Protocol` that allowing to generate for us the specifications of this one, it also requires that we specify a model and a separator (cf. [guide](protocols.html#textual-protocol)).

## Creating the PacketHandler

The next step is to create the packet handler.

Here, for the example, we will be able to receive 2 types of packets:

- `PING`: a simple packet to test if our server is responding
- `LOGIN username password`: a packet allowing you to authenticate yourself

NOTE: I'll later add a mix task (`mix elven.gen.packet`) in order to automatically generate and add handlers to the projet.

Let's create a new file at `lib/login_server/packet_handler.ex` :

```elixir
defmodule LoginServer.PacketHandler do
  @moduledoc false

  use ElvenGard.PacketHandler

  alias LoginServer.Auth.Actions

  @desc """
  The PING packet: a simple packet to test if our server is responding
  """
  packet "PING" do
    resolve &Actions.ping/3
  end

  @desc """
  The LOGIN packet: a packet allowing you to authenticate yourself
  
  NOTE: This packet returns the IP and port of our world server before disconnecting the client
  """
  packet "LOGIN" do
    @desc "An user name"
    field :username, :string
    
    @desc "A plaintext password"
    field :password, :string
    
    resolve &Actions.player_connect/3
  end
end
```

Now that we are able to accept a client, receive his packets and parse them, we need to process these packets and send a response back to the client.  
This is the role of [Actions](actions.html) and [Views](views.html).

## Creating Views

In order to create our Views, let's first create the `lib/login_server/auth` folder and add the `views.ex` file with the following content:

```elixir
defmodule LoginServer.Auth.Views do
  @moduledoc false

  use ElvenGard.View

  @impl ElvenGard.View
  def render(:pong, _) do
    "PONG"
  end

  @impl ElvenGard.View
  def render(:login_error, %{error: error}) do
    "ERROR #{error}"
  end

  @impl ElvenGard.View
  def render(:login_success, %{ip: ip, port: port}) do
    "SUCCESS #{ip}:#{port}"
  end
end
```

This file being quite simple, I wouldn't dwell on it (a guide is available [here](views.html) for more information).

## Creating Actions

Last step before we can test our server: the creation of our [Actions](actions.html).  

To do this, create the file `lib/login_server/auth/actions.ex` with the following content :

```elixir
defmodule LoginServer.Auth.Actions do
  @moduledoc false

  alias ElvenGard.Socket
  alias LoginServer.Auth.Views

  @doc false
  @spec ping(Socket.t(), String.t(), map) :: String.t()
  def ping(socket, _header, _params) do
    render = Views.render(:pong, nil)
    
    # Send the response to the client
    Socket.send(socket, render)
    
    # We continue to receive packets from the client
    {:cont, socket}
  end
  
  @doc false
  @spec player_connect(Socket.t(), String.t(), map) :: String.t()
  def player_connect(socket, _header, params) do
    %{
      username: username,
      password: password
    } = params

    render =
      if username == "admin" and password == "pass" do
        Views.render(:login_success, %{ip: "127.0.0.1", port: 1234})
      else
        Views.render(:login_error, %{error: "Bad credentials!"})
      end

    # Send the response to the client
    Socket.send(socket, render)

    # Requests the client's disconnection, stating that there has been no error
    {:halt, {:ok, :normal}, socket}
  end
end
```

This file is also quite explicit, so I encourage you to check the documentation [here](actions.html) if you want more details.

## Testing our server

As our server is based on a text protocol, we just need to connect to port 3000 in TCP (using `netcat` for example) and send our messages.

```
# Terminal 1
$> mix run --no-halt
XX:XX:XX.XXX [info]  Login server started on port 3000
```

```
# Terminal 2
$> nc 127.0.0.1 3000
PING
PONG                    # Client response
LOGIN admin test
ERROR Bad credentials!  # Client response

# Here we have been disconnected by the action of the LOGIN packet. So we need to reconnect.

$> nc 127.0.0.1 3000
LOGIN admin pass
SUCCESS 127.0.0.1:1234  # Client response

# We are once again disconnected after receiving the "SUCCESS" packet
```

```
# Back to terminal 1, we also received several messages due to the application logs
XX:XX:XX.XX [info]  Login server started on port 3000
XX:XX:XX.XX [info]  New connection: 226167fe-493f-47ea-ae81-5f94e0e728ed
XX:XX:XX.XX [info]  New message from 226167fe-493f-47ea-ae81-5f94e0e728ed (len: 5)
XX:XX:XX.XX [info]  New message from 226167fe-493f-47ea-ae81-5f94e0e728ed (len: 17)
XX:XX:XX.XX [info]  226167fe-493f-47ea-ae81-5f94e0e728ed is now disconnected (reason: :normal)
XX:XX:XX.XX [info]  New connection: 728760aa-024a-4a04-a5b3-251bd1780c77
XX:XX:XX.XX [info]  New message from 728760aa-024a-4a04-a5b3-251bd1780c77 (len: 17)
XX:XX:XX.XX [info]  728760aa-024a-4a04-a5b3-251bd1780c77 is now disconnected (reason: :normal)
```
