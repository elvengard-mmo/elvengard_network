defmodule ElvenGard.Network.Endpoint.Ranch do
  @moduledoc """
  Endpoint adapter using Ranch
  """

  @behaviour ElvenGard.Network.Endpoint

  ## Endpoint behaviour

  @impl true
  def start_link(opts) do
    IO.inspect(opts)
  end
end
