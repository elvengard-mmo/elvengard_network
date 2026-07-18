if Code.ensure_loaded?(ThousandIsland.Socket) do
  defmodule ElvenGard.Network.Socket.Adapters.ThousandIsland do
    @moduledoc false

    @behaviour ElvenGard.Network.Socket.Adapter

    @type options :: [socket: ThousandIsland.Socket.t()]
    @type state :: ThousandIsland.Socket.t()

    ## Socket.Adapter callbacks

    @impl true
    @spec new(options()) :: state()
    def new(options) do
      Keyword.fetch!(options, :socket)
    end

    @impl true
    @spec send(state(), iodata()) :: ThousandIsland.Transport.on_send()
    def send(socket, data) do
      ThousandIsland.Socket.send(socket, data)
    end

    @impl true
    @spec setopts(state(), ThousandIsland.Transport.socket_set_options()) ::
            ThousandIsland.Transport.on_setopts()
    def setopts(socket, opts) do
      ThousandIsland.Socket.setopts(socket, opts)
    end

    @impl true
    @spec close(state()) :: ThousandIsland.Transport.on_close()
    def close(socket) do
      ThousandIsland.Socket.close(socket)
    end
  end
end
