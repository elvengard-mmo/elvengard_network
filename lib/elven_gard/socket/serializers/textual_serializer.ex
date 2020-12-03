defmodule ElvenGard.Socket.TextualSerializer do
  @moduledoc """
  WARN: This module will soon be deprecated.
  """

  @doc """
  Same as c:ElvenGard.Protocol.encode/2
  """
  @callback handle_encode(data :: any(), socket :: Socket.t()) :: binary()

  @doc """
  TODO: Documentation
  """
  @callback handle_decode(data :: binary(), socket :: Socket.t()) :: any()

  @aliases [
    integer: ElvenGard.Protocol.Textual.IntegerType,
    float: ElvenGard.Protocol.Textual.FloatType,
    string: ElvenGard.Protocol.Textual.StringType
  ]
end
