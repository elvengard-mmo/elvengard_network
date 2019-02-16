defmodule LoginServer.Actions.Auth do
  @moduledoc """
  TODO: Too much changes on this Module....
  """

  alias ElvenGard.Structures.Client
  alias LoginServer.Crypto

  @client_version Application.get_env(:login_server, :client_version)

  @spec player_connect(Client.t(), map) :: {:halt, any, Client.t()}
  def player_connect(ctx, params) do
    with %{
           username: user,
           password: enc_password,
           version: version
         } <- params,
         :ok <- check_version(version),
         {:ok, password} <- decrypt_password(enc_password),
         {:ok, account_id} <- get_account_id(user, password),
         {:ok, server_list} <- get_server_list(),
         {:ok, session} <- create_session(account_id, user, password),
         packet <- create_res_packet(session, server_list) do
      {:halt, {:ok, packet}, ctx}
    else
      err ->
        {:halt, err, ctx}
    end
  end

  #
  # Private function
  #

  @doc false
  @spec check_version(String.t()) :: :ok | {:error, :TOO_OLD}
  defp check_version(version) do
    if version == @client_version, do: :ok, else: {:error, :TOO_OLD}
  end

  @spec decrypt_password(String.t()) :: {:ok, String.t()}
  defp decrypt_password(enc_pass) do
    pass = Crypto.decrypt_pass(enc_pass)
    {:ok, pass}
  end

  @doc false
  @spec get_account_id(String.t(), String.t()) ::
          {:ok, integer} | {:error, :BAD_CREDENTIALS}
  defp get_account_id(username, password) do
    # case Accounts.get_account_id(username, password) do
    #   nil -> {:error, :BAD_CREDENTIALS}
    #   account_id -> {:ok, account_id}
    # end

    if username == "admin" and password == "admin" do
      {:ok, 1}
    else
      {:error, :BAD_CREDENTIALS}
    end
  end

  @doc false
  @spec get_server_list() :: {:ok, String.t()} | {:error, :MAINTENANCE}
  defp get_server_list() do
    # case WorldManager.Interfaces.server_list() do
    #   nil -> {:error, :MAINTENANCE}
    #   server_list -> {:ok, server_list}
    # end

    server_list = "127.0.0.1:5000:0:1.1.SomeTest -1:-1:-1:10000.10000.1"
    {:ok, server_list}
  end

  @doc false
  @spec create_session(non_neg_integer, String.t(), String.t()) ::
          {:ok, String.t()} | {:error, :ID_ALREADY_USE}
  defp create_session(_account_id, _username, _password) do
    # case SessionManager.Interfaces.create_session(account_id, username, password) do
    #   {:ok, session} -> {:ok, session}
    #   _ -> {:error, :ID_ALREADY_USE}
    # end

    {:ok, 123}
  end

  @doc false
  @spec create_res_packet(integer, String.t()) :: String.t()
  defp create_res_packet(session, server_list) do
    "NsTeST #{session} #{server_list}"
  end
end
