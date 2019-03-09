defmodule ElvenGard.Structures.FieldDocumentation do
  @moduledoc """
  Structure for a packet field's documentation
  """

  @keys [:name, :type, :description]
  @enforce_keys @keys
  defstruct @keys

  @type t :: %__MODULE__{}
  @type empty_string :: String.t() | nil

  @doc """
  Create a new structure
  """
  @spec new(String.t(), atom, empty_string) :: __MODULE__.t()
  def new(name, type, desc) do
    %__MODULE__{
      name: name,
      type: type,
      description: if(desc, do: String.trim(desc), else: desc)
    }
  end
end
