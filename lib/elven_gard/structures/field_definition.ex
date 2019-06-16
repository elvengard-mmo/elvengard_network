defmodule ElvenGard.Structures.FieldDefinition do
  @moduledoc """
  Structure for a packet field's definition
  """

  @keys [:name, :type, :description, :opts]
  @enforce_keys @keys
  defstruct @keys

  @type t :: %__MODULE__{}
  @type empty_string :: String.t() | nil

  @doc """
  Create a new structure
  """
  @spec new(String.t(), atom, empty_string, list) :: __MODULE__.t()
  def new(name, type, desc, opts) do
    %__MODULE__{
      name: name,
      type: type,
      opts: opts,
      description: if(desc, do: String.trim(desc), else: desc)
    }
  end
end
