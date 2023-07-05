defmodule ElvenGard.Network.Type do
  @moduledoc ~S"""
  Define a behaviour for custom types (packet parsing)
  """

  @doc "Decode a field type"
  @callback decode(raw :: bitstring, opts :: keyword) :: {any, remaining :: bitstring}

  @doc "Encode a field type"
  @callback encode(value :: any, opts :: keyword) :: bitstring
end
