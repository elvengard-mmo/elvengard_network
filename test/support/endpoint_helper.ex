defmodule ElvenGard.Network.EndpointHelper do
  @moduledoc """
  Utility functions for integration testing endpoints.
  """

  @spec get_unused_port_number() :: 1..65536
  def get_unused_port_number() do
    listen_on_os_assigned_port()
    |> get_port_number_and_close()
  end

  defp listen_on_os_assigned_port() do
    {:ok, socket} = :gen_tcp.listen(0, [])
    socket
  end

  defp get_port_number_and_close(socket) do
    {:ok, port_number} = :inet.port(socket)
    :gen_tcp.close(socket)
    port_number
  end
end
