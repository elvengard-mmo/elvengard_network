defmodule ElvenGard.PacketHandler.PacketDefinition do
  @moduledoc ~S"""
  Structure for a packet's definition
  """

  alias ElvenGard.{PacketHandler, Socket}
  alias ElvenGard.PacketHandler.FieldDefinition

  @enforce_keys [:header]

  defstruct header: nil,
            description: nil,
            resolver: nil,
            fields: [],
            tags: []

  @type nullable_string :: String.t() | nil
  @type resolver_result :: PacketHandler.callback_response()
  @type resolver :: (header :: any(), args :: map(), socket :: Socket.t() -> resolver_result())

  @type t :: %__MODULE__{
          header: any(),
          description: nullable_string(),
          resolver: resolver() | nil,
          fields: [FieldDefinition.t(), ...],
          tags: [atom(), ...]
        }

  @doc """
  Create a new structure
  """
  @spec new(String.t(), nullable_string(), [atom(), ...]) :: __MODULE__.t()
  def new(header, desc \\ nil, tags \\ []) do
    %__MODULE__{
      header: header,
      description: norm_description(desc),
      tags: tags
    }
  end

  @doc """
  Add a field
  """
  @spec add_field(__MODULE__.t(), FieldDefinition.t()) :: __MODULE__.t()
  def add_field(%__MODULE__{} = def, %FieldDefinition{} = field) do
    new_fields = def.fields ++ [field]
    %__MODULE__{def | fields: new_fields}
  end

  @doc """
  Add a tag
  """
  @spec add_tag(__MODULE__.t(), atom) :: __MODULE__.t()
  def add_tag(%__MODULE__{} = def, tag) do
    new_tags = def.tags ++ [tag]
    %__MODULE__{def | tags: new_tags}
  end

  @doc """
  Set the `description` value for the given structure
  """
  @spec set_description(__MODULE__.t(), nullable_string()) :: __MODULE__.t()
  def set_description(%__MODULE__{} = def, desc) do
    %__MODULE__{def | description: norm_description(desc)}
  end

  @doc """
  Set the `resolver` value for the given structure
  """
  @spec set_resolver(__MODULE__.t(), resolver()) :: __MODULE__.t()
  def set_resolver(%__MODULE__{} = def, resolver) do
    %__MODULE__{def | resolver: resolver}
  end

  ## Privates function

  @doc false
  @spec norm_description(nullable_string()) :: nullable_string()
  defp norm_description(nil), do: nil
  defp norm_description(str), do: String.trim(str)
end
