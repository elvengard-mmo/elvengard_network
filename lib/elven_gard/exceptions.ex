defmodule ElvenGard.UnknownViewError do
  @moduledoc """
  Exception raised when a view is not found
  """

  defexception [:parent, :type]

  @impl true
  def message(%{parent: parent, type: type}) do
    "unable to find the `render/2` function with the '#{inspect(type)}' key " <>
      "(from #{inspect(parent)})"
  end
end

defmodule ElvenGard.FieldTypeError do
  @moduledoc """
  Exception raised when the type of a protocol field is not defined
  """

  defexception [:field_name, :field_type, :packet_name]

  @impl true
  def message(%{field_name: field_name, field_type: field_type, packet_name: packet_name}) do
    "invalid type #{inspect(field_type)} for the field #{inspect(field_name)} " <>
      "(packet: #{inspect(packet_name)})"
  end
end
