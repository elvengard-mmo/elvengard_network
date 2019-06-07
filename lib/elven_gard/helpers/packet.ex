defmodule ElvenGard.Helpers.Packet do
  @moduledoc """
  Define some usefull macros for create a packet that can be handled
  by an `ElvenGard.Helpers.Frontend`
  """

  alias ElvenGard.Structures.{FieldDefinition, PacketDefinition}

  @anno if :erlang.system_info(:otp_release) >= '19', do: [generated: true], else: [line: -1]

  @doc false
  defmacro __using__(_) do
    parent = __MODULE__
    caller = __CALLER__.module

    Module.register_attribute(
      caller,
      :elven_packet_definitions,
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
    # handle_packet/3 to be reported in case the user has defined a catch-all
    # handle_packet/3 clause.
    #
    # Thanks to Phoenix
    # https://github.com/phoenixframework/phoenix/blob/master/lib/phoenix/view.ex
    quote @anno do
      def handle_packet(packet_name, args, client) do
        Logger.warn("Unknown packet header #{inspect(packet_name)} with args: #{inspect(args)}")
        {:cont, client}
      end

      def get_packet_definitions() do
        @elven_packet_definitions
      end
    end
  end

  @doc """
  Define a new packet
  """
  defmacro packet(packet_name, do: exp) do
    def_packet(__CALLER__.module, packet_name)

    quote do
      @elven_current_packet PacketDefinition.new(unquote(packet_name), @desc)
      @desc nil

      unquote(exp)
    end
  end

  @doc """
  Define an useless packet

  ¯\\_(ツ)_/¯
  """
  defmacro useless_packet(packet_name) do
    caller = __CALLER__.module

    quote do
      @elven_current_packet PacketDefinition.new(unquote(packet_name), @desc)
      @elven_current_packet PacketDefinition.add_tag(@elven_current_packet, :useless_packet)
      Module.put_attribute(unquote(caller), :elven_packet_definitions, @elven_current_packet)
      @desc nil

      @doc false
      def handle_packet(unquote(packet_name), args, client) do
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
    # handle_packet/3 to be reported in case the user doesn't use
    # `packet_name`, `args`, or `client`
    #
    # Thanks to Phoenix
    # https://github.com/phoenixframework/phoenix/blob/master/lib/phoenix/view.ex
    quote @anno do
      def handle_packet(var!(packet_name), var!(args), var!(client)) do
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

    quote do
      desc = @desc || Keyword.get(unquote(opts), :description)

      @elven_current_packet PacketDefinition.add_field(
                              @elven_current_packet,
                              FieldDefinition.new(
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
  Define the resolver function
  """
  defmacro resolve(fun) do
    caller = __CALLER__.module
    packet_name = Module.get_attribute(caller, :elven_packet_name)

    quote do
      Module.put_attribute(unquote(caller), :elven_packet_definitions, @elven_current_packet)

      @doc false
      def handle_packet(unquote(packet_name), args, client) do
        unquote(fun).(client, unquote(packet_name), args)
      end
    end
  end

  #
  # Private functions
  #

  # TODO: Replace theses hard coded modules by a function "aliases"
  @doc false
  @spec type_module(atom) :: atom
  defp type_module(:byte), do: ElvenGard.Types.Binary.ByteType
  defp type_module(:integer), do: ElvenGard.Types.Binary.IntegerType
  defp type_module(:long), do: ElvenGard.Types.Binary.LongType
  defp type_module(:padding), do: ElvenGard.Types.Binary.PaddingType
  defp type_module(:short), do: ElvenGard.Types.Binary.ShortType
  defp type_module(:string), do: ElvenGard.Types.Binary.StringType
  defp type_module(type), do: type

  @doc false
  @spec def_packet(atom, binary) :: term
  defp def_packet(caller, packet_name) do
    # Register new attributes
    Module.register_attribute(caller, :elven_packet_name, [])

    # Save the packet type
    Module.put_attribute(caller, :elven_packet_name, packet_name)
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
