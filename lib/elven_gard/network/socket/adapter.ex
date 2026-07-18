defmodule ElvenGard.Network.Socket.Adapter do
  @moduledoc """
  Defines the transport operations used by `ElvenGard.Network.Socket`.

  An adapter owns the transport-specific state and translates the common socket
  operations to the underlying networking library.
  """

  @type options :: Keyword.t()
  @type state :: term()

  @callback new(options()) :: state()
  @callback send(state(), iodata()) :: :ok | {:error, term()}
  @callback setopts(state(), list()) :: :ok | {:error, term()}
  @callback close(state()) :: :ok
end
