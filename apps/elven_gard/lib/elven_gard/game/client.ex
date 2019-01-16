defmodule ElvenGard.Game.Client do
  @moduledoc """
  Documentation for ElvenGard.Game.Client.
  """

  @keys [:id, :socket, :transport]
  @enforce_keys @keys
  defstruct @keys

  @type t :: %__MODULE__{}

  @spec new(identifier, atom) :: __MODULE__.t()
  def new(socket, transport) do
    %__MODULE__{
      id: UUID.uuid4(),
      socket: socket,
      transport: transport
    }
  end

  @spec send(__MODULE__.t(), binary) :: :ok | {:error, atom}
  def send(client, message) do
    %__MODULE__{
      socket: socket,
      transport: transport
    } = client

    transport.send(socket, message)
  end
end
