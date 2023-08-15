defmodule ElvenGard.Network.UnknownViewError do
  @moduledoc ~S"""
  Exception raised when a View is not found
  """

  defexception [:parent, :type]

  @impl true
  def message(%{parent: parent, type: type}) do
    "unable to find the `render/2` function with the '#{inspect(type)}' key " <>
      "(in #{inspect(parent)})"
  end
end
