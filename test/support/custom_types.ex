defmodule ElvenGard.Network.CustomTypes do
  defmodule Boolean do
    use ElvenGard.Network.Type

    @type t :: boolean

    ## Behaviour impls

    @impl true
    @spec decode(bitstring, keyword) :: {t(), bitstring}
    def decode(data, _opts) when is_binary(data) do
      {value, rest} =
        case String.split(data, " ", parts: 2) do
          [value, rest] -> {value, rest}
          [value] -> {value, ""}
        end

      if value not in ["0", "1"], do: raise("invalid boolean value: #{inspect(value)}")
      {value == "1", rest}
    end

    @impl true
    @spec encode(t(), keyword) :: bitstring
    def encode(data, _opts) when is_boolean(data) do
      if data, do: "1", else: "0"
    end
  end

  defmodule Str do
    use ElvenGard.Network.Type

    @type t :: String.t()

    ## Behaviour impls

    @impl true
    @spec decode(bitstring, keyword) :: {t(), bitstring}
    def decode(data, opts) when is_binary(data) do
      fill? = opts[:fill] == true
      do_decode(data, fill?)
    end

    @impl true
    @spec encode(t(), keyword) :: bitstring
    def encode(data, _opts) when is_binary(data), do: data

    ## Private functions

    defp do_decode(data, true), do: {data, ""}

    defp do_decode(data, false) do
      case String.split(data, " ", parts: 2) do
        [value, rest] -> {value, rest}
        [value] -> {value, ""}
      end
    end
  end

  defmodule Int do
    use ElvenGard.Network.Type

    @type t :: integer

    ## Behaviour impls

    @impl true
    @spec decode(bitstring, keyword) :: {t(), bitstring}
    def decode(data, _opts) when is_binary(data) do
      case String.split(data, " ", parts: 2) do
        [value, rest] -> {String.to_integer(value, 10), rest}
        [value] -> {String.to_integer(value, 10), ""}
      end
    end

    @impl true
    @spec encode(t(), keyword) :: bitstring
    def encode(data, _opts) when is_integer(data), do: Integer.to_string(data)
  end

  defmodule Date do
    use ElvenGard.Network.Type

    @type t :: Elixir.Date.t()

    ## Behaviour impls

    @impl true
    @spec decode(bitstring, keyword) :: {t(), bitstring}
    def decode(data, _opts) when is_binary(data) do
      case String.split(data, " ", parts: 2) do
        [value, rest] -> {Elixir.Date.from_iso8601!(value), rest}
        [value] -> {Elixir.Date.from_iso8601!(value), ""}
      end
    end

    @impl true
    @spec encode(t(), keyword) :: bitstring
    def encode(%Elixir.Date{} = data, _opts), do: Elixir.Date.to_string(data)
  end
end
