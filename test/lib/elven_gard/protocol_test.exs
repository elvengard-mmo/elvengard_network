defmodule ElvenGard.ProtocolTest do
  use ExUnit.Case

  defmodule BasicType do
    use ElvenGard.Type

    @impl ElvenGard.Type
    def encode(val, _opts), do: val

    @impl ElvenGard.Type
    def decode(val, _opts), do: val
  end

  describe "Protocol raise error if:" do
    test "encode/1 is not defined" do
      needle = ~r"function encode/1 required by behaviour"

      assert_raise RuntimeError, needle, fn ->
        defmodule Test do
          use ElvenGard.Protocol

          def decode(x), do: x
        end
      end
    end

    test "decode/1 is not defined" do
      needle = ~r"function decode/1 required by behaviour"

      assert_raise RuntimeError, needle, fn ->
        defmodule Test do
          use ElvenGard.Protocol

          def encode(x), do: x
        end
      end
    end
  end
end
