defmodule MyApp.PingPacket do
  use ElvenGard.View.Packet

  defstruct []

  @type t :: %__MODULE__{}

  @impl true
  def serialize(_data, _opts), do: "PING"
end

defmodule MyApp.HelloPacket do
  use ElvenGard.View.Packet

  defstruct [:msg]

  @type t :: %__MODULE__{msg: any()}

  @impl true
  def serialize(data, separator: sep) do
    ["HELLO", sep, serialize(data.msg)]
  end
end

defmodule MyApp.SimpleView do
  use ElvenGard.View

  @impl true
  def render(:ping, _), do: %MyApp.PingPacket{}
  def render(:hello, %{msg: msg}), do: %MyApp.HelloPacket{msg: msg}
end
