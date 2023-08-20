defmodule MinecraftEx do
  @moduledoc """
  Documentation for `MinecraftEx`.
  """

  defguard has_state(socket, state) when socket.assigns.state == state
end
