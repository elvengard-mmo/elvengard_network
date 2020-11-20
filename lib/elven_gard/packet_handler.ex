defmodule ElvenGard.PacketHandler do
  @moduledoc """
  Define a DSL to help create packet definitions

  Define a domain specific language (DSL) so you can create packet definitions more easily.  
  Theses packets can be handled by a `ElvenGard.Frontend` and can be used as model to generate
  specs for a Protocol (cf. `ElvenGard.Protocol.Textual` for example). Protocols can use
  metadata created by the DSL to know the packet definitions.

  WARN: add invalid arguments to the handle_packet/3 callback can match an invalid handler.  
  For example if we have a packet handler defined like that:

    packet "LOGIN" do
      field :username, :string
      field :password, :string
      resolve &__MODULE__.connect/3
    end

  Here, the correct way to call our packet handler is:

    args = %{username: "user", password: "password"}
    handle_packet("LOGIN", args, nil)

  However, the following code will also match this handler:

    args = %{username: "user", password: "password", invalid_arg: 123}
    handle_packet("LOGIN", args, nil)

  I'm not currently sure if it's a bug or a feature.
  """

  alias ElvenGard.Structures.{Client, FieldDefinition, PacketDefinition}

  @type header() :: any()
  @type args() :: map()
  @type socket() :: Client.t()
  @type reason() :: term()
  @type callback_response() :: {:cont, socket()} | {:halt, reason(), socket()}

  @callback handle_packet(header(), args(), socket()) :: callback_response()
  @callback handle_ignore(header(), args(), socket()) :: callback_response()

  ## Public API

  @doc false
  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__), only: :macros

      @behaviour unquote(__MODULE__)
      @before_compile unquote(__MODULE__)

      unquote(prelude())
    end
  end

  @doc """
  Define a new packet handler
  """
  defmacro packet(header, do: exp) do
    quote do
      unquote(create_definition(header))
      unquote(exp)
      unquote(persist_definition())
    end
  end

  @doc """
  Define a new field for a packet
  """
  defmacro field(name, type, opts \\ []) do
    expanded_type = Macro.expand(type, __CALLER__)

    quote do
      unquote(add_field(name, expanded_type, opts))
    end
  end

  @doc """
  Define the resolver function
  """
  defmacro resolve(callback) do
    quote do
      unquote(create_resolver(callback))
    end
  end

  @doc """
  Define an unused packet
  """
  defmacro ignore_packet(header) do
    quote do
      unquote(create_definition(header, [:ignored]))
      unquote(persist_definition())

      unquote(create_ignore_handler(header))
    end
  end

  @doc """
  Override the default callback if there is no clause matching `c:handle_packet/3`
  """
  defmacro default_packet_callback(do: exp) do
    quote do
      unquote(create_callback(:handle_packet, exp))
    end
  end

  @doc """
  Override the default callback for an unused packet
  """
  defmacro ignore_packet_callback(do: exp) do
    quote do
      unquote(create_callback(:handle_ignore, exp))
    end
  end

  ## Internal macros

  @doc false
  defmacro __before_compile__(env) do
    definitions = env.module |> Module.get_attribute(:elven_defs) |> Enum.reverse()

    quote do
      unquote(create_introspection_helpers(definitions))
      unquote(create_default_callbacks())
    end
  end

  ## Private functions

  @doc false
  defp prelude() do
    quote do
      # Module.register_attribute(__MODULE__, :elven_packet, [])
      # Module.register_attribute(__MODULE__, :elven_header, [])
      Module.register_attribute(__MODULE__, :elven_args, accumulate: true)
      Module.register_attribute(__MODULE__, :elven_defs, accumulate: true)
      Module.register_attribute(__MODULE__, :desc, [])
    end
  end

  @doc false
  defp create_introspection_helpers(definitions) do
    quote do
      @doc false
      def __defs__(), do: unquote(Macro.escape(definitions))
    end
  end

  @doc false
  defp create_default_callbacks() do
    # We are using `generated: true` because we don't want warnings coming from
    # handle_packet/3 or handle_ignore/2 to be reported in case the user has
    # defined a catch-all clause.
    quote generated: true do
      def handle_packet(header, _args, socket), do: {:halt, {:invalid, header}, socket}
      def handle_ignore(_header, _args, socket), do: {:cont, socket}
    end
  end

  @doc false
  defp create_callback(name, exp) do
    callback = Macro.escape(exp)

    # We are using `generated: true` because we don't want warnings coming from
    # callback/3 to be reported in case the user doesn't use `header`, `args`,
    # or `socket` variables
    quote generated: true, unquote: false, bind_quoted: [callback: callback, name: name] do
      def unquote(name)(var!(header), var!(args), var!(socket)) do
        unquote(callback)
      end
    end
  end

  @doc false
  defp create_definition(header, tags \\ []) do
    quote do
      @elven_header unquote(header)
      @elven_packet PacketDefinition.new(unquote(header), @desc, unquote(tags))
      @desc nil
    end
  end

  @doc false
  defp persist_definition() do
    quote do
      @elven_defs @elven_packet
    end
  end

  @doc false
  defp add_field(name, type, opts) do
    quote do
      @elven_packet PacketDefinition.add_field(
                      @elven_packet,
                      FieldDefinition.new(
                        unquote(name),
                        unquote(type),
                        @desc,
                        unquote(opts)
                      )
                    )

      @desc nil
    end
  end

  @doc false
  defp create_ignore_handler(header) do
    quote do
      def handle_packet(unquote(header), args, socket) do
        handle_ignore(unquote(header), args, socket)
      end
    end
  end

  @doc false
  defp create_resolver(callback) do
    escaped_cb = Macro.escape(callback)

    quote unquote: false, bind_quoted: [escaped_cb: escaped_cb, parent: __MODULE__] do
      # FIXME: Better way to do this ??
      args_ast = parent.gen_ast_for_fields(@elven_packet.fields)

      def handle_packet(unquote(@elven_header), unquote(args_ast) = args, socket) do
        unquote(escaped_cb).(unquote(@elven_header), args, socket)
      end
    end
  end

  @doc false
  # TODO: This function should (probably) be private
  def gen_ast_for_fields(args) do
    args_map =
      args
      |> Enum.reverse()
      |> Enum.reduce([], &ast_for_field/2)

    {:%{}, [], args_map}
  end

  @doc false
  defp ast_for_field(%FieldDefinition{name: name} = def, acc) do
    is_optional = Keyword.get(def.opts, :optional)
    match = Keyword.get(def.opts, :match)

    case {is_optional, match} do
      {true, _} -> acc
      {_, nil} -> [{name, {:_, [], Elixir}} | acc]
      {_, val} -> [{name, val} | acc]
    end
  end
end
