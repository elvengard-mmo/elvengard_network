defmodule ElvenGard.Packet do
  @moduledoc """
  Define some usefull macros for create a packet that can be handled
  by an `ElvenGard.Frontend`
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
      import unquote(parent)

      require Logger

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

      def fetch_definitions() do
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
  Define an unused packet

  ¯\\\\_(ツ)_/¯
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
    expanded_type = Macro.expand(type, __CALLER__)

    quote do
      desc = @desc || Keyword.get(unquote(opts), :description)

      @elven_current_packet PacketDefinition.add_field(
                              @elven_current_packet,
                              FieldDefinition.new(
                                unquote(name),
                                unquote(expanded_type),
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

    quote unquote: false, bind_quoted: [caller: caller, fun: fun, packet_name: packet_name] do
      Module.put_attribute(caller, :elven_packet_definitions, @elven_current_packet)

      params_map =
        Enum.map(@elven_current_packet.fields, fn %FieldDefinition{name: name} = x ->
          case Keyword.get(x.opts, :using) do
            nil -> {name, {:_, [], Elixir}}
            val -> {name, val}
          end
        end)

      @doc false
      def handle_packet(
            unquote(packet_name),
            unquote({:%{}, [], params_map}) = args,
            client
          ) do
        unquote(fun).(client, unquote(packet_name), args)
      end
    end
  end

  #
  # Private functions
  #

  @doc false
  @spec def_packet(atom, binary) :: term
  defp def_packet(caller, packet_name) do
    # Register new attributes
    Module.register_attribute(caller, :elven_packet_name, [])

    # Save the packet type
    Module.put_attribute(caller, :elven_packet_name, packet_name)
  end
end
