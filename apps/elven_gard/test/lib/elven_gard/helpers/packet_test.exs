defmodule ElvenGard.Helpers.PacketTest do
  use ExUnit.Case

  alias ElvenGard.Structures.{FieldDocumentation, PacketDocumentation}

  defmodule HandlersA do
    use ElvenGard.Helpers.Packet

    packet :invalid_packet, do: :ok
  end

  defmodule HandlersB do
    use ElvenGard.Helpers.Packet

    packet :very_basic_packet do
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersC do
    use ElvenGard.Helpers.Packet

    @desc "Some desc"
    packet :packet_with_desc do
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersD do
    use ElvenGard.Helpers.Packet

    @desc """
    Some

    multi lines

    desc
    """
    packet :packet_with_multi_lines_desc do
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersE do
    use ElvenGard.Helpers.Packet

    @real_desc "Here is the real description"
    @desc @real_desc
    packet :packet_desc_previously_set do
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersF do
    use ElvenGard.Helpers.Packet

    packet :packet_no_desc_with_fields_no_desc do
      field :first_field, :string
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersG do
    use ElvenGard.Helpers.Packet

    packet :packet_no_desc_with_fields_desc_attr do
      @desc "Description attribute"
      field :first_field, :integer
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersH do
    use ElvenGard.Helpers.Packet

    packet :packet_no_desc_with_fields_desc_opts do
      field :first_field, :string, description: "Description in options"
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersI do
    use ElvenGard.Helpers.Packet

    packet :packet_no_desc_with_fields_multi_line_desc do
      @desc """
      Some

      multi lines

      desc
      """
      field :first_field, :string
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersJ do
    use ElvenGard.Helpers.Packet

    @real_desc "Here is the real description"

    packet :packet_no_desc_with_fields_desc_prev_set do
      @desc @real_desc
      field :first_field, :string
      resolve fn (_, _) -> :ok end
    end
  end

  defmodule HandlersK do
    use ElvenGard.Helpers.Packet

    useless_packet :useless_packet_no_desc
  end

  defmodule HandlersL do
    use ElvenGard.Helpers.Packet

    @desc "Some description"
    useless_packet :useless_packet_desc
  end

  defmodule HandlersZ do
    use ElvenGard.Helpers.Packet

    @desc "Don't know what is this packet"
    useless_packet :useless_packet

    @desc "Simple login packet"
    packet :login do
      field :username, :string
      field :password, :string

      @desc "Seems to be always `NONE`"
      field :unknown1, :string

      @desc "Unknown too. Maybe a random number ?"
      field :unknown2, :integer

      resolve &handle_login/2
    end

    packet :mov do
      field :x, :integer
      field :y, :integer
      resolve &handle_mov/2
    end

    defp handle_login(_, _), do: :ok
    defp handle_mov(_, _), do: :ok
  end

  describe "Invalid packet:" do
    test "no documentation" do
      got = HandlersA.elven_get_packet_documentation()
      expected = []

      assert expected == got
    end
  end

  describe "No field, single packet defined with" do
    test "no documentation" do
      got = HandlersB.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: nil,
          fields: [],
          name: :very_basic_packet,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentation" do
      got = HandlersC.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: "Some desc",
          fields: [],
          name: :packet_with_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "multiline documentation" do
      got = HandlersD.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: "Some\n\nmulti lines\n\ndesc",
          fields: [],
          name: :packet_with_multi_lines_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentations previously set" do
      got = HandlersE.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: "Here is the real description",
          fields: [],
          name: :packet_desc_previously_set,
          tags: []
        }
      ]

      assert expected == got
    end
  end

  describe "Single packet, single field defined with" do
    test "no documentation" do
      got = HandlersF.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: nil,
          fields: [
            %FieldDocumentation{
              description: nil,
              name: :first_field,
              type: :string
            }
          ],
          name: :packet_no_desc_with_fields_no_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "attribute documentation" do
      got = HandlersG.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: nil,
          fields: [
            %FieldDocumentation{
              description: "Description attribute",
              name: :first_field,
              type: :integer
            }
          ],
          name: :packet_no_desc_with_fields_desc_attr,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentation option" do
      got = HandlersH.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: nil,
          fields: [
            %FieldDocumentation{
              description: "Description in options",
              name: :first_field,
              type: :string
            }
          ],
          name: :packet_no_desc_with_fields_desc_opts,
          tags: []
        }
      ]

      assert expected == got
    end

    test "multiline documentation" do
      got = HandlersI.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: nil,
          fields: [
            %FieldDocumentation{
              description: "Some\n\nmulti lines\n\ndesc",
              name: :first_field,
              type: :string
            }
          ],
          name: :packet_no_desc_with_fields_multi_line_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentations previously set" do
      got = HandlersJ.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: nil,
          fields: [
            %FieldDocumentation{
              description: "Here is the real description",
              name: :first_field,
              type: :string
            }
          ],
          name: :packet_no_desc_with_fields_desc_prev_set,
          tags: []
        }
      ]

      assert expected == got
    end
  end

  describe "Tagged packet" do
    test "no documentation" do
      got = HandlersK.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: nil,
          fields: [],
          name: :useless_packet_no_desc,
          tags: [:useless_packet]
        }
      ]

      assert expected == got
    end

    test "documentation" do
      got = HandlersL.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          description: "Some description",
          fields: [],
          name: :useless_packet_desc,
          tags: [:useless_packet]
        }
      ]
      assert expected == got
    end
  end

  describe "Final test:" do
    test "Multiple packet, multiple fields, tagged or not, documented or not" do
      got = HandlersZ.elven_get_packet_documentation()
      expected = [
        %PacketDocumentation{
          name: :mov,
          description: nil,
          tags: []
          fields: [
            %FieldDocumentation{
              description: nil,
              name: :x,
              type: :integer
            },
            %FieldDocumentation{
              description: nil,
              name: :y,
              type: :integer
            }
          ],
        },
        %PacketDocumentation{
          name: :login,
          description: "Simple login packet",
          tags: []
          fields: [
            %FieldDocumentation{
              description: nil,
              name: :username,
              type: :string
            },
            %FieldDocumentation{
              description: nil,
              name: :password,
              type: :string
            },
            %FieldDocumentation{
              description: "Seems to be always `NONE`",
              name: :unknown1,
              type: :string
            },
            %FieldDocumentation{
              description: "Unknown too. Maybe a random number ?",
              name: :unknown2,
              type: :integer
            }
          ],
        },
        %PacketDocumentation{
          name: :useless_packet,
          description: "Don't know what is this packet",
          tags: [:useless_packet]
          fields: [],
        }
      ]

      assert expected == got
    end
  end
end
