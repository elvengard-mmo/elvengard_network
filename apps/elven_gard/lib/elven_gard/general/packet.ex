defmodule ElvenGard.General.Packet do
  @moduledoc """
  Documentation for ElvenGard.General.Packet.

  TODO: Doccument all macros and functions.
  """

  defmacro __using__(_) do
    parent = __MODULE__
    caller = __CALLER__.module
    Module.put_attribute(caller, :elven_default_function, false)

    quote do
      require Logger
      import unquote(parent)

      @before_compile unquote(parent)
    end
  end

  defmacro __before_compile__(env) do
    default? = Module.get_attribute(env.module, :elven_default_function)

    case default? do
      true ->
        # Same than `quote do end`
        {:__block__, [], []}

      false ->
        create_default_handle()
    end
  end

  defmacro packet(packet_type, do: exp) do
    def_packet(__CALLER__.module, packet_type)

    quote do
      unquote(exp)
    end
  end

  defmacro default_packet(do: exp) do
    caller = __CALLER__.module
    Module.put_attribute(caller, :elven_default_function, true)

    quote do
      def handle_packet([var!(packet_type) | var!(args)], var!(ctx)) do
        unquote(exp)
      end
    end
  end

  defmacro field(name, type \\ :string) do
    caller = __CALLER__.module
    Module.put_attribute(caller, :elven_params, {name, type})

    # Same than `quote do end`
    {:__block__, [], []}
  end

  defmacro resolve(fun) do
    caller = __CALLER__.module
    packet_type = Module.get_attribute(caller, :elven_packet_type)

    params =
      Module.get_attribute(caller, :elven_params)
      |> Enum.reverse()
      |> check_types!(packet_type)

    quote do
      @doc false
      def handle_packet([unquote(packet_type) | args], ctx) do
        zip_params = Enum.zip(unquote(params), args)
        fin_params = Enum.map(zip_params, &parse_type!/1) |> Enum.into(%{})
        unquote(fun).(ctx, fin_params)
      end
    end
  end

  #
  # Some functions
  #

  @available_types [:string, :integer]

  @doc false
  @spec parse_type!({{atom, atom}, String.t()}) :: {atom, String.t() | integer}
  def parse_type!({{name, :string}, val}), do: {name, val}

  def parse_type!({{name, :integer}, val}) do
    {name, String.to_integer(val, 10)}
  end

  #
  # Private functions
  #

  @doc false
  @spec def_packet(atom, binary) :: :ok
  defp def_packet(caller, packet_type) do
    # Delete old attributes
    Module.delete_attribute(caller, :elven_params)

    # Register the new one
    Module.register_attribute(caller, :elven_params, accumulate: true)
    Module.register_attribute(caller, :elven_packet_type, [])

    # Save the packet type
    Module.put_attribute(caller, :elven_packet_type, packet_type)

    # Remove Dialyzer warning
    :ok
  end

  @spec create_default_handle() :: term
  defp create_default_handle() do
    quote do
      def handle_packet([packet_type | args], ctx) do
        Logger.warn("Unknown packet header #{inspect(packet_type)} with args: #{inspect(args)}")
        {:halt, {:error, {:unknown_header, packet_type}}, ctx}
      end
    end
  end

  @doc false
  @spec check_types!(list(tuple), binary) :: list(tuple)
  defp check_types!(args, packet_type) do
    Enum.each(args, &do_check_types!(&1, packet_type))
    args
  end

  @doc false
  @spec check_types!(tuple, binary) :: no_return
  defp do_check_types!({name, type}, packet_type) do
    case type do
      x when x in @available_types -> :ok
      _ -> raise "Unknown type '#{type}' for '#{name}' inside packet '#{packet_type}'"
    end
  end
end
