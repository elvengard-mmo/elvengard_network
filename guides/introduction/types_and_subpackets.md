# Types and Sub Packets

In this section, we will learn how to use `ElvenGard.Network.Type`.

We're going to create all the types and subpackets needed to serialize and deserialize 
our packets according to our [network protocol](network_protocol.html).

## Types

So, according to our network protocol, we must define theses types:

| Field Type | Encoded or Decoded | Used by                   |
|------------|--------------------|---------------------------|
| String     | Both               | LoginRequest, LoginFailed |
| DateTime   | Encoded            | PongResponse              |

Let's start with the first one:

```elixir
# file: lib/login_server/types/string_type.ex
defmodule LoginServer.Types.StringType do
  @moduledoc """
  Documentation for LoginServer.Types.StringType
  """

  use ElvenGard.Network.Type

  @type t :: String.t()

  ## Behaviour impls

  @impl true
  @spec decode(binary(), Keyword.t()) :: {t(), binary()}
  def decode(data, _opts) when is_binary(data) do
    case String.split(data, " ", parts: 2) do
      [string] -> {string, ""}
      [string, rest] -> {string, rest}
    end
  end

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(data, _opts) when is_binary(data) do
    data
  end
end
```

As you can see, we just need to use `ElvenGard.Network.Type` and define 2 callbacks:

  - `decode/2`: takes the binary to be decoded and options. This callback must return a 
    tuple in the form `{type_decoded, rest_of_binary_not_decoded}`.
  - `encode/2`: takes the type to encode and options. This callback must return an
    [iodata](https://hexdocs.pm/elixir/IO.html#module-io-data) representation of our type.

**NOTE**: Typespecs and guards are not mandatory, but are a good practice.

```elixir
# file: lib/login_server/types/date_time_type.ex
defmodule LoginServer.Types.DateTimeType do
  @moduledoc """
  Documentation for LoginServer.Types.DateTimeType
  """

  use ElvenGard.Network.Type

  @type t :: DateTime.t()

  ## Behaviour impls

  @impl true
  def decode(_data, _opts), do: raise("unimplemented")

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(data, _opts) when is_struct(data, DateTime) do
    DateTime.to_string(data)
  end
end
```

For this second type it's the same, the only difference is that we don't need to need 
to deserialize a `DateTimeType`, so we can just ignore the function and raise if someone 
is trying to use it.

## Sub Packets

A Sub Packet is simply a type that uses other types. Unlike a packet, it has no ID, as it 
will be used by other packets.

Let's look at a simple example with the `WorldInfo`:

| SubPacket Name | Encoded or Decoded | Used by      |
|----------------|--------------------|--------------|
| WorldInfo      | Encoded            | LoginSucceed |

```elixir
# file: lib/login_server/sub_packets/world_info.ex
defmodule LoginServer.SubPackets.WorldInfo do
  @moduledoc """
  Documentation for LoginServer.SubPackets.WorldInfo
  """

  use ElvenGard.Network.Type

  alias __MODULE__
  alias LoginServer.Types.{IntegerType, StringType}

  @enforce_keys [:host, :port]
  defstruct [:host, :port]

  @type t :: %WorldInfo{host: StringType.t(), port: IntegerType.t()}

  ## Behaviour impls

  @impl true
  def decode(_data, _opts), do: raise("unimplemented")

  @impl true
  @spec encode(t(), Keyword.t()) :: iolist()
  def encode(data, opts) when is_struct(data, WorldInfo) do
    separator = Keyword.fetch!(opts, :sep)

    [
      StringType.encode(data.host),
      StringType.encode(separator),
      IntegerType.encode(data.port)
    ]
  end
end
```

As you can see, this type is represented in Elixir by a structure with 2 mandatory fields: 
`host` and `port`. They are each represented by the `StringType` and `IntegerType` types, 
with which they will be encoded.

**NOTE**: this Sub Packet has another special feature: the separator used by the fields can be 
configured through options. We'll see how to use it in the next sections.

As the `IntegerType` type has not been created yet, let's create it defining only the 
`encode/2` function.

```elixir
# file: lib/login_server/types/integer_type.ex
defmodule LoginServer.Types.IntegerType do
  @moduledoc """
  Documentation for LoginServer.Types.IntegerType
  """

  use ElvenGard.Network.Type

  @type t :: integer()

  ## Behaviour impls

  @impl true
  def decode(_data, _opts), do: raise("unimplemented")

  @impl true
  @spec encode(t(), Keyword.t()) :: binary()
  def encode(data, _opts) when is_integer(data) do
    Integer.to_string(data)
  end
end
```

## Summary

In this guide, we've seen how to create customizable types and sub-packets. Now we'll look at 
how to use them.
