defmodule ElvenGard.Structures.PacketDocumentation do
  @moduledoc """
  Structure for a packet's documentation
  """

  @keys [:name, :packetdoc, :fields, :tags]
  @enforce_keys @keys
  defstruct @keys

  alias ElvenGard.Structures.FieldDocumentation

  @type t :: %__MODULE__{}

  @doc """
  Create a new structure
  """
  @spec new(String.t()) :: __MODULE__.t()
  def new(name) do
    %__MODULE__{name: name, packetdoc: nil, fields: [], tags: []}
  end

  @doc """
  Add a field
  """
  @spec add_field(__MODULE__.t(), FieldDocumentation.t()) :: __MODULE__.t()
  def add_field(doc_struct, %FieldDocumentation{} = field) do
    %__MODULE__{fields: fields} = doc_struct

    new_fields = fields ++ [field]
    %__MODULE__{doc_struct | fields: new_fields}
  end

  @doc """
  Add a tag
  """
  @spec add_tag(__MODULE__.t(), atom) :: __MODULE__.t()
  def add_tag(doc_struct, tag) do
    %__MODULE__{tags: tags} = doc_struct

    new_tags = tags ++ [tag]
    %__MODULE__{doc_struct | tags: new_tags}
  end

  @doc """
  The the `packetdoc` value for the given structure
  """
  @spec set_packetdoc(__MODULE__.t(), String.t()) :: __MODULE__.t()
  def set_packetdoc(doc_struct, packetdoc) do
    %__MODULE__{doc_struct | packetdoc: packetdoc}
  end
end
