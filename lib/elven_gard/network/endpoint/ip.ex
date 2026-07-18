defmodule ElvenGard.Network.Endpoint.IP do
  @moduledoc false

  alias ElvenGard.Network.Endpoint

  ## Public API

  @spec normalize!(Endpoint.ip_address()) :: :inet.ip_address()
  def normalize!(ip) do
    case ip do
      ip when is_tuple(ip) -> ip
      ip when is_binary(ip) -> parse!(ip)
    end
  end

  ## Private function

  defp parse!(ip) do
    case ip |> String.to_charlist() |> :inet.parse_address() do
      {:ok, address} ->
        address

      {:error, reason} ->
        raise ArgumentError, "invalid IP address #{inspect(ip)}: #{inspect(reason)}"
    end
  end
end
