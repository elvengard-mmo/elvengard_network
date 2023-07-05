defmodule MinecraftEx.Resources do
  @moduledoc """
  Handles app resources
  """

  @priv_dir :code.priv_dir(:minecraft_ex)

  @favicon_path Path.join(@priv_dir, "favicon.png")
  @external_resource @favicon_path
  @favicon_base64 @favicon_path |> File.read!() |> Base.encode64()

  def favicon(), do: "data:image/png;base64," <> @favicon_base64
end
