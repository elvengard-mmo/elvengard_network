if Code.ensure_loaded?(:ranch) do
  defmodule ElvenGard.Network.Socket.Adapters.Ranch do
    @moduledoc false

    @behaviour ElvenGard.Network.Socket.Adapter

    @type options :: [transport: module(), socket: :ranch_transport.socket()]
    @type state :: {transport :: module(), socket :: :ranch_transport.socket()}

    @impl true
    @spec new(options()) :: state()
    def new(options) do
      transport = Keyword.fetch!(options, :transport)
      socket = Keyword.fetch!(options, :socket)

      {transport, socket}
    end

    @impl true
    def send({transport, socket}, data) do
      transport.send(socket, data)
    end

    @impl true
    def setopts({transport, socket}, opts) do
      transport.setopts(socket, opts)
    end

    @impl true
    def close({transport, socket}) do
      transport.close(socket)
    end
  end
end
