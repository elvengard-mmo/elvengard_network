defmodule ElvenGard.Socket.DummySerializer do
  @moduledoc ~S"""
  It is the simplest serializer.

  The `encode!` and `decode!` callbacks return the data 
  without modifying it.
  """

  @behaviour ElvenGard.Socket.Serializer

  @impl true
  def encode!(data, _opts), do: data

  @impl true
  def decode!(message, _assigns), do: message
end
