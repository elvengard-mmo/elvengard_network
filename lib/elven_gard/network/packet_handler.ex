defmodule ElvenGard.Network.PacketHandler do
  @moduledoc """
  Provides a behavior for handling incoming packets.

  This module defines the callback `handle_packet/2`, which must be implemented by
  modules using this behavior to process incoming packets.

  For detailed usage information, please refer to the
  [packet handler guide](https://hexdocs.pm/elvengard_network/packet_handler.html).
  """

  alias ElvenGard.Network.Socket

  @type halt_reason :: any()

  @doc """
  This callback function is called after a packet is received and decoded by the Endpoint.

  It is responsible for processing the packet data and returning one of the following tuples:

  - `{:cont, socket}`: Indicates that packet processing is complete, and the client connection
    should continue.
  - `{:halt, socket}`: Indicates that packet processing is complete, and the client connection
    should be terminated.
  - `{:halt, reason, socket}`: Same as `{:halt, socket}`, passing `reason` to the protocol's
    `c:ElvenGard.Network.SocketHandler.handle_halt/2` callback.

  ## Parameters

    - `packet`: The packet data as a struct.
    - `socket`: The socket representing the client connection.
  """
  @callback handle_packet(packet :: struct(), socket :: Socket.t()) ::
              {:cont, Socket.t()}
              | {:halt, Socket.t()}
              | {:halt, reason :: halt_reason(), Socket.t()}
end
