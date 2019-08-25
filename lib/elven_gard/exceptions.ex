defmodule ElvenGard.TypeOptionError do
  @moduledoc """
  Raised when wrong options are given to `c:ElvenGard.Type.encode/2` or `c:ElvenGard.Type.decode/2`
  """
  defexception [:message]
end

defmodule ElvenGard.UnknownViewError do
  @moduledoc """
  Raised when the view is not found
  """
  defexception [:parent, :type]

  @impl true
  def message(%{parent: parent, type: type}) do
    "Unable to find the `render/2` function with the '#{inspect(type)}' key " <>
      "(from #{inspect(parent)})"
  end
end
