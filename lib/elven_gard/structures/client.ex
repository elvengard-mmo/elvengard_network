defmodule ElvenGard.Structures.Client do
  @moduledoc """
  Manage a socket.
  You can store some metadata on this structure.
  """

  @keys [:id, :socket, :transport, :metadata]
  @enforce_keys @keys
  defstruct @keys ++ [encoder: nil]

  @type t :: %__MODULE__{}
  @type metadata_key :: [term, ...] | term
  @type metadata_value :: term | nil

  @doc """
  Create a new structure
  """
  @spec new(identifier, atom, map) :: __MODULE__.t()
  def new(socket, transport, metadata \\ %{}) do
    %__MODULE__{
      id: UUID.uuid4(),
      socket: socket,
      transport: transport,
      metadata: metadata
    }
  end

  @doc """
  Send a packet to the client
  """
  @spec send(__MODULE__.t(), term) :: :ok | {:error, atom}
  def send(client, message) do
    %__MODULE__{
      socket: socket,
      transport: transport,
      encoder: encoder
    } = client

    encoded_message = encoder.encode(message)
    transport.send(socket, encoded_message)
  end

  @doc """
  Try to get a metadata embeded in the structure.
  You can use an array for a deep research.
  """
  @spec get_metadata(__MODULE__.t(), metadata_key) :: metadata_value
  def get_metadata(client, [_ | _] = key), do: get_in(client.metadata, key)
  def get_metadata(client, key), do: Map.get(client.metadata, key)

  @doc """
  Put a metadata in the structure.
  """
  @spec put_metadata(__MODULE__.t(), metadata_key, metadata_value) :: __MODULE__.t()
  def put_metadata(client, [_ | _] = key, value) do
    %__MODULE__{
      metadata: metadata
    } = client

    new_meta = put_in(metadata, key, value)
    %__MODULE__{client | metadata: new_meta}
  end

  def put_metadata(client, key, value) do
    %__MODULE__{
      metadata: metadata
    } = client

    new_meta = Map.put(metadata, key, value)
    %__MODULE__{client | metadata: new_meta}
  end
end
