defmodule WorldServer.Actions.Auth do
  @moduledoc """
  TODO: Documentation for WorldServer.Actions.Auth
  """

  alias ElvenGard.Structures.Client

  @spec process_session_id(Client.t(), %{session_id: integer}) :: {:cont, Client.t()}
  def process_session_id(client, params) do
    new_client =
      client
      |> Client.put_metadata(:session_id, params.session_id)
      |> Client.put_metadata(:auth_step, :waiting_username)

    {:cont, new_client}
  end

  @spec process_username(Client.t(), %{username: String.t()}) :: {:cont, Client.t()}
  def process_username(client, params) do
    new_client =
      client
      |> Client.put_metadata(:username, params.username)
      |> Client.put_metadata(:auth_step, :waiting_password)

    {:cont, new_client}
  end

  @spec process_password(Client.t(), %{password: String.t()}) :: {:cont, Client.t()}
  def process_password(client, params) do
    new_client =
      client
      |> Client.put_metadata(:password, params.password)
      |> Client.put_metadata(:auth_step, :done)

    # TODO: Check credentials and session_id here
    {:cont, new_client}
  end
end
