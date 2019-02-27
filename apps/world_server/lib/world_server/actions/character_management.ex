defmodule WorldServer.Actions.CharacterManagement do
  @moduledoc """
  TODO: Documentation for WorldServer.Actions.CharacterManagement
  """

  alias ElvenGard.Structures.Client

  @doc false
  @spec send_character_list(Client.t(), map) :: :ok
  def send_character_list(%Client{} = client, _params) do
    characters = [
      %{
        name: "DarkyZ",
        slot: 1,
        gender: 1,
        hair_style: 1,
        hair_color: 1
      }
    ]

    Client.send(client, "clist_start 0")
    Enum.each(characters, &do_send_character_list(client, &1))
    Client.send(client, "clist_end")
    :ok
  end

  #
  # Private functions
  #

  @doc false
  defp do_send_character_list(%Client{} = client, %{} = character) do
    %{
      name: name,
      slot: slot,
      gender: gender,
      hair_style: hair_style,
      hair_color: hair_color
    } = character

    class = 0
    level = 30
    job_level = 10
    hero_level = 99
    equipments = "-1.-1.-1.-1.-1.-1.-1.-1"
    pets = "-1"

    packet =
      "clist #{slot} #{name} 0 #{gender} #{hair_style} #{hair_color} 0 #{class}" <>
        " #{level} #{hero_level} #{equipments} #{job_level} 1 1 #{pets} 0"

    Client.send(client, packet)
  end
end
