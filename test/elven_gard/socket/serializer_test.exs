Code.require_file("../../fixtures/serializers.exs", __DIR__)

defmodule ElvenGard.Socket.SerializerTest do
  use ExUnit.Case, async: true

  describe "alias_for/1" do
    test "returns an alias if exists" do
      assert MyApp.BasicSerializer.alias_for(:foo) == MyApp.Bar
      assert MyApp.BasicSerializer.alias_for(:abc) == MyApp.Baz
    end

    test "returns the given value if no alias" do
      assert MyApp.BasicSerializer.alias_for(:foo1) == :foo1
      assert MyApp.BasicSerializer.alias_for(MyApp.Baz) == MyApp.Baz
    end
  end

  describe "alias_for!/1" do
    test "returns an alias if exists" do
      assert MyApp.BasicSerializer.alias_for!(:foo) == MyApp.Bar
      assert MyApp.BasicSerializer.alias_for!(:abc) == MyApp.Baz
    end

    test "raises an error if no alias was found" do
      assert_raise KeyError, fn -> MyApp.BasicSerializer.alias_for!(:foo1) end
      assert_raise KeyError, fn -> MyApp.BasicSerializer.alias_for!(MyApp.Baz) end
    end
  end
end
