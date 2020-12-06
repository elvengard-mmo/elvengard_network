Code.require_file("../../../fixtures/views.exs", __DIR__)
Code.require_file("../../../fixtures/serializers.exs", __DIR__)

defmodule ElvenGard.Socket.TextualSerializerTest do
  use ExUnit.Case, async: true

  describe "aliases/0" do
    test "is defined" do
      assert is_list(MyApp.SimpleTextSerializer.aliases())
    end
  end

  describe "encode!/2" do
    test "can encode a builtin term" do
      assert encode_packet("HEADER") == "HEADER"
      assert encode_packet(:foo) == "foo"
      assert encode_packet(123) == "123"
    end

    test "can encode structs" do
      assert encode_packet(%MyApp.PingPacket{}) == "PING"

      assert encode_packet(%MyApp.HelloPacket{}) == "HELLO nil"
      assert encode_packet(%MyApp.HelloPacket{msg: "WORLD"}) == "HELLO WORLD"
      assert encode_packet(%MyApp.HelloPacket{msg: 123}) == "HELLO 123"
    end
  end

  # describe "decode!/2" do
  #   @tag :skip
  #   test "returns an empty list for an empty packet" do
  #     assert MyApp.SimpleTextSerializer.decode!("", %{}) == []
  #   end

  #   @tag :skip
  #   test "can decode a simple packet" do
  #     assert MyApp.SimpleTextSerializer.decode!("PING", %{}) == [{"PING", %{}}]
  #   end
  # end

  ## Helpers

  defp encode_packet(packet) do
    packet
    |> MyApp.SimpleTextSerializer.encode!([])
    |> IO.iodata_to_binary()
  end
end
