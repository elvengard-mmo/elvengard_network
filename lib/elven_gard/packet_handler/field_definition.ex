defmodule ElvenGard.PacketHandler.FieldDefinition do
  @moduledoc """
  Structure for a packet field's definition
  """

  @enforce_keys [:name, :type]

  defstruct name: nil,
            type: nil,
            description: nil,
            opts: []

  @type nullable_string :: String.t() | nil

  @type t :: %__MODULE__{
          name: String.t(),
          type: atom(),
          description: nullable_string(),
          opts: keyword()
        }

  @doc """
  Create a new structure
  """
  @spec new(String.t(), atom(), nullable_string(), keyword()) :: __MODULE__.t()
  def new(name, type, desc, opts) do
    %__MODULE__{
      name: name,
      type: type,
      opts: opts,
      description: norm_description(desc)
    }
  end

  ## Privates function

  @doc false
  @spec norm_description(nullable_string()) :: nullable_string()
  defp norm_description(nil), do: nil
  defp norm_description(str), do: String.trim(str)
end
