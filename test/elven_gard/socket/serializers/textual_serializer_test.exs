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

    test "Raise Protocol.UndefinedError if protocol not implemented" do
      assert_raise Protocol.UndefinedError, fn ->
        encode_packet(%{})
      end
    end
  end

  describe "decode!/2" do
    test "returns an empty list for an empty packet" do
      assert decode_packet("") == []
    end

    test "can decode a packet without args" do
      assert decode_packet("PING") == [{"PING", %{}}]
    end

    test "can decode a packet with args" do
      assert decode_packet("LOGIN admin test") ==
               [{"LOGIN", %{"username" => "admin", "password" => "test"}}]

      assert decode_packet("PING 10") == [{"PING", %{"count" => 10}}]
    end

    test "raise an error if cannot decode args (malformed packet)" do
      assert_raise RuntimeError, ~r"unable to decode args", fn ->
        decode_packet("INVALID_HEADER 1")
      end
    end
  end

  ## Helpers

  defp encode_packet(packet) do
    packet
    |> MyApp.SimpleTextSerializer.encode!([])
    |> IO.iodata_to_binary()
  end

  defp decode_packet(packet) do
    MyApp.SimpleTextSerializer.decode!(packet, %{})
  end
end
