defmodule ElvenGard.ProtocolTest do
  use ExUnit.Case, async: true

  describe "Protocol raise error if:" do
    test "encode/1 is not defined" do
      needle = ~r"function encode/1 required by behaviour"

      assert_raise RuntimeError, needle, fn ->
        defmodule ProtocolWithoutEncode do
          use ElvenGard.Protocol

          def decode(x), do: x
        end
      end
    end

    test "decode/1 is not defined" do
      needle = ~r"function decode/1 required by behaviour"

      assert_raise RuntimeError, needle, fn ->
        defmodule ProtocolWithoutDecode do
          use ElvenGard.Protocol

          def encode(x), do: x
        end
      end
    end
  end
end
