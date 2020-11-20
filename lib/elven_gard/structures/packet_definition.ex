defmodule ElvenGard.Structures.PacketDefinition do
  @moduledoc """
  Structure for a packet's definition
  """

  alias ElvenGard.Structures.FieldDefinition

  @enforce_keys [:header]

  defstruct header: nil,
            description: nil,
            resolver: nil,
            fields: [],
            tags: []

  @type description() :: String.t() | nil
  @type resolver() :: any()

  @type t() :: %__MODULE__{
          header: any(),
          description: description(),
          resolver: resolver(),
          fields: [FieldDefinition.t(), ...],
          tags: [atom(), ...]
        }

  @doc """
  Create a new structure
  """
  @spec new(String.t(), description(), [atom(), ...]) :: __MODULE__.t()
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
  @spec set_description(__MODULE__.t(), description()) :: __MODULE__.t()
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
  @spec norm_description(String.t()) :: String.t()
  defp norm_description(str) do
    if str, do: String.trim(str), else: str
  end
end
