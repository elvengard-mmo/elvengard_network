defmodule ElvenGard.TypeTest do
  use ExUnit.Case

  defmodule BasicType do
    use ElvenGard.Type

    @impl ElvenGard.Type
    def encode(val, _opts), do: val

    @impl ElvenGard.Type
    def decode(val, _opts), do: val
  end

  describe "Type behaviour defines:" do
    test "encode/1" do
      assert :erlang.function_exported(BasicType, :encode, 1)
    end

    test "decode/1" do
      assert :erlang.function_exported(BasicType, :decode, 1)
    end
  end
end
