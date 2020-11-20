Code.require_file("../fixtures/custom_types.exs", __DIR__)

defmodule ElvenGard.TypeTest do
  use ExUnit.Case, async: true

  describe "__using__/1" do
    test "generate encode/1 which redirects on encode/2" do
      assert MyApp.BasicType.encode(123) == {123, []}
    end

    test "generate decode/1 which redirects on encode/2" do
      assert MyApp.BasicType.decode(456) == {456, []}
    end
  end
end
