defmodule ElvenGard.Network.Socket.Adapter do
  @moduledoc """
  Defines the transport operations used by `ElvenGard.Network.Socket`.

  An adapter owns the transport-specific state and translates the common socket
  operations to the underlying networking library.
  """

  @type error_reason :: any()
  @type options :: Keyword.t()
  @type state :: any()

  @callback new(options()) :: state()
  @callback send(state(), iodata()) :: :ok | {:error, error_reason()}
  @callback setopts(state(), list()) :: :ok | {:error, error_reason()}
  @callback close(state()) :: :ok | {:error, error_reason()}
end
