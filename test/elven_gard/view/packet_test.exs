Code.require_file("../../fixtures/views.exs", __DIR__)

defmodule ElvenGard.View.PacketTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  describe "Packet" do
    test "raise if no structure was defined" do
      assert_raise ArgumentError, ~r"must define a structure", fn ->
        defmodule PacketWithoutStruct do
          use ElvenGard.View.Packet
        end
      end
    end

    test "must warn if no typespec was defined for the structure" do
      create_packet = fn ->
        defmodule PacketWithoutTypeSpec do
          use ElvenGard.View.Packet

          defstruct foo: nil
        end
      end

      assert capture_io(:stderr, create_packet) =~ "no typespec found for"
    end
  end
end
