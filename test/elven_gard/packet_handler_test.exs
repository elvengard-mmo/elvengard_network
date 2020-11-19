Code.require_file("../fixtures/packet_handlers.exs", __DIR__)

defmodule ElvenGard.PacketHandlerTest do
  use ExUnit.Case, async: true

  ## General behaviour

  describe "__using__/1" do
    test "generate a default packet callback" do
      assert MyApp.EmptyPacketHandler.handle_packet("UNKNOWN", %{}, 123) ==
               {:halt, {:invalid, "UNKNOWN"}, 123}
    end

    test "generate a default ignore callback" do
      assert MyApp.EmptyPacketHandler.handle_ignore("USELESS", %{}, 123) == {:cont, 123}
    end
  end

  ## Ignore callbacks

  describe "ignore_packet/1" do
    test "delegate to `c:handle_ignore/3` callback" do
      assert MyApp.DefaultIgnorePacketHandler.handle_packet("USELESS", %{}, 123) == {:cont, 123}
    end
  end

  describe "ignore_packet_callback/1" do
    test "can override the default ignore callback" do
      assert simple_handle_packet("USELESS", %{}, 123) == {:halt, {"USELESS", %{}, 123}}
    end
  end

  ## Packet callbacks

  describe "default_packet_callback/1" do
    test "can override the default packet handler" do
      assert simple_handle_packet("UNKNOWN", %{}, 123) == {:cont, {"UNKNOWN", %{}, 123}}
    end
  end

  describe "resolve/1" do
    test "create a handle_packet/3 callback" do
      assert simple_handle_packet("PING", %{}, nil) == {:ping, "PING", %{}, nil}
    end
  end

  describe "field/3" do
    test "pattern match with args names" do
      args = %{username: "username", password: "password"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect
    end

    test "pattern match same header but differents args names" do
      args = %{username: "username", token_2fa: "token_2fa"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_2fa
    end

    test "pattern match same header but less args" do
      args = %{username: "username"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_user
    end

    test "pattern match same header but with matching arg" do
      args = %{username: "admin", password: "password"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_admin
    end

    test "pattern match same header but with optional arg" do
      args = %{username: "admin"}
      assert simple_handle_packet("LOGIN", args, 123) == :connect_admin
    end
  end

  ## TODO: Test documentation & definitions generator

  ## Helpers

  defp simple_handle_packet(header, args, socket) do
    MyApp.SimplePacketHandler.handle_packet(header, args, socket)
  end
end
