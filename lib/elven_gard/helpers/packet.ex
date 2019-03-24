defmodule ElvenGard.Helpers.Packet do
  @moduledoc """
  Define some usefull macros for create a packet that can be handled
  by an `ElvenGard.Helpers.Frontend`
  """

  alias ElvenGard.Structures.{FieldDocumentation, PacketDocumentation}

  @anno if :erlang.system_info(:otp_release) >= '19', do: [generated: true], else: [line: -1]

  @doc false
  defmacro __using__(_) do
    parent = __MODULE__
    caller = __CALLER__.module

    Module.register_attribute(
      caller,
      :elven_packet_documentations,
      accumulate: true,
      persist: true
    )

    quote do
      require Logger
      import unquote(parent)

      @before_compile unquote(parent)
      @desc nil
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    # We are using @anno because we don't want warnings coming from
    # handle_packet/2 to be reported in case the user has defined a catch-all
    # handle_packet/2 clause.
    #
    # Thanks to Phoenix
    # https://github.com/phoenixframework/phoenix/blob/master/lib/phoenix/view.ex
    quote @anno do
      def handle_packet([packet_name | args], client) do
        Logger.warn("Unknown packet header #{inspect(packet_name)} with args: #{inspect(args)}")
        {:cont, client}
      end

      def elven_get_packet_documentation() do
        @elven_packet_documentations
      end
    end
  end

  @doc """
  Define a new packet
  """
  defmacro packet(packet_name, do: exp) do
    def_packet(__CALLER__.module, packet_name)

    quote do
      @elven_current_packet PacketDocumentation.new(unquote(packet_name), @desc)
      @desc nil

      unquote(exp)
    end
  end

  @doc """
  Define an useless packet

  ¯\_(ツ)_/¯
  """
  defmacro useless_packet(packet_name) do
    caller = __CALLER__.module

    quote do
      @elven_current_packet PacketDocumentation.new(unquote(packet_name), @desc)
      @elven_current_packet PacketDocumentation.add_tag(@elven_current_packet, :useless_packet)
      Module.put_attribute(unquote(caller), :elven_packet_documentations, @elven_current_packet)
      @desc nil

      @doc false
      def handle_packet([unquote(packet_name) | args], client) do
        Logger.info(
          "Discarded packet header #{inspect(unquote(packet_name))} with args: #{inspect(args)}"
        )

        {:cont, client}
      end
    end
  end

  @doc """
  Define the default behaviour if no packet's header match
  """
  defmacro default_packet(do: exp) do
    # We are using @anno because we don't want warnings coming from
    # handle_packet/2 to be reported in case the user doesn't use
    # `packet_name`, `args`, or `client`
    #
    # Thanks to Phoenix
    # https://github.com/phoenixframework/phoenix/blob/master/lib/phoenix/view.ex
    quote @anno do
      def handle_packet([var!(packet_name) | var!(args)], var!(client)) do
        unquote(exp)
      end
    end
  end

  @doc """
  Define a new field for a packet
  """
  defmacro field(name, type, opts \\ []) do
    caller = __CALLER__.module
    packet_name = Module.get_attribute(caller, :elven_packet_name)

    real_type =
      type
      |> Macro.expand(__CALLER__)
      |> type_module()

    check_type!({name, real_type}, packet_name)
    Module.put_attribute(caller, :elven_params, {name, real_type})

    quote do
      desc = @desc || Keyword.get(unquote(opts), :description)

      @elven_current_packet PacketDocumentation.add_field(
                              @elven_current_packet,
                              FieldDocumentation.new(
                                unquote(name),
                                unquote(real_type),
                                desc,
                                Keyword.delete(unquote(opts), :description)
                              )
                            )

      @desc nil
    end
  end

  @doc """
  Define the handler function
  """
  defmacro resolve(fun) do
    caller = __CALLER__.module
    packet_name = Module.get_attribute(caller, :elven_packet_name)

    params =
      caller
      |> Module.get_attribute(:elven_params)
      |> Enum.reverse()

    quote do
      Module.put_attribute(unquote(caller), :elven_packet_documentations, @elven_current_packet)

      @doc false
      def handle_packet([unquote(packet_name) | args], client) do
        zip_params = Enum.zip(unquote(params), args)
        fin_params = Enum.into(zip_params, %{}, &parse_type/1)
        unquote(fun).(client, fin_params)
      end
    end
  end

  #
  # Some functions
  #

  @doc false
  @spec type_module(atom) :: atom
  defp type_module(:byte), do: ElvenGard.Types.ElvenByte
  defp type_module(:integer), do: ElvenGard.Types.ElvenInteger
  defp type_module(:long), do: ElvenGard.Types.ElvenLong
  defp type_module(:padding), do: ElvenGard.Types.ElvenPadding
  defp type_module(:short), do: ElvenGard.Types.ElvenShort
  defp type_module(:string), do: ElvenGard.Types.ElvenString
  defp type_module(type), do: type

  @doc """
  Currently, can only parse strings and integers.

  TODO: Add custom field support (like integer bytes/bits length,
  user custom field etc...)
  """
  @spec parse_type({{atom, atom}, String.t()}) :: {atom, String.t() | integer}
  def parse_type({{name, :string}, val}), do: {name, val}

  def parse_type({{name, :integer}, val}) do
    {name, String.to_integer(val, 10)}
  end

  #
  # Private functions
  #

  @doc false
  @spec def_packet(atom, binary) :: :ok
  defp def_packet(caller, packet_name) do
    # Delete old attributes
    Module.delete_attribute(caller, :elven_params)

    # Register the new one
    Module.register_attribute(caller, :elven_params, accumulate: true)
    Module.register_attribute(caller, :elven_packet_name, [])

    # Save the packet type
    Module.put_attribute(caller, :elven_packet_name, packet_name)

    # Remove Dialyzer warning
    :ok
  end

  @doc false
  @spec check_type!(tuple, binary) :: term
  defp check_type!({name, type}, packet_name) do
    unless Keyword.has_key?(type.__info__(:functions), :decode) do
      raise "Unknown type '#{inspect(type)}' for '#{inspect(name)}' " <>
              "for packet '#{inspect(packet_name)}'"
    end
  end
end
