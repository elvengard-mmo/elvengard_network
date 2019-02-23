defmodule ElvenGard.Structures.FieldDocumentation do
  @moduledoc """
  Structure for a packet field's documentation
  """

  @keys [:name, :type, :doc]
  @enforce_keys @keys
  defstruct @keys

  @type t :: %__MODULE__{}

  @doc """
  Create a new structure
  """
  @spec new(String.t(), atom, String.t()) :: __MODULE__.t()
  def new(name, type, doc) do
    %__MODULE__{name: name, type: type, doc: doc}
  end
end
