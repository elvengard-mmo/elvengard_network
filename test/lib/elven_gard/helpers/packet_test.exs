defmodule ElvenGard.Helpers.PacketTest do
  use ExUnit.Case

  alias ElvenGard.Structures.{FieldDefinition, PacketDefinition}

  defmodule HandlersA do
    use ElvenGard.Helpers.Packet

    packet :invalid_packet, do: :ok
  end

  defmodule HandlersB do
    use ElvenGard.Helpers.Packet

    packet :very_basic_packet do
      resolve fn _, _ -> :ok end
    end
  end

  defmodule HandlersC do
    use ElvenGard.Helpers.Packet

    @desc "Some desc"
    packet :packet_with_desc do
      resolve fn _, _ -> :ok end
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
      resolve fn _, _ -> :ok end
    end
  end

  defmodule HandlersE do
    use ElvenGard.Helpers.Packet

    @real_desc "Here is the real description"
    @desc @real_desc
    packet :packet_desc_previously_set do
      resolve fn _, _ -> :ok end
    end
  end

  defmodule HandlersF do
    use ElvenGard.Helpers.Packet

    packet :packet_no_desc_with_fields_no_desc do
      field :first_field, :string
      resolve fn _, _ -> :ok end
    end
  end

  defmodule HandlersG do
    use ElvenGard.Helpers.Packet

    packet :packet_no_desc_with_fields_desc_attr do
      @desc "Description attribute"
      field :first_field, :integer
      resolve fn _, _ -> :ok end
    end
  end

  defmodule HandlersH do
    use ElvenGard.Helpers.Packet

    packet :packet_no_desc_with_fields_desc_opts do
      field :first_field, :string, description: "Description in options"
      resolve fn _, _ -> :ok end
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
      resolve fn _, _ -> :ok end
    end
  end

  defmodule HandlersJ do
    use ElvenGard.Helpers.Packet

    @real_desc "Here is the real description"

    packet :packet_no_desc_with_fields_desc_prev_set do
      @desc @real_desc
      field :first_field, :string
      resolve fn _, _ -> :ok end
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
      got = HandlersA.get_packet_definitions()
      expected = []

      assert expected == got
    end
  end

  describe "No field, single packet defined with" do
    test "no documentation" do
      got = HandlersB.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: nil,
          fields: [],
          name: :very_basic_packet,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentation" do
      got = HandlersC.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: "Some desc",
          fields: [],
          name: :packet_with_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "multiline documentation" do
      got = HandlersD.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: "Some\n\nmulti lines\n\ndesc",
          fields: [],
          name: :packet_with_multi_lines_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentations previously set" do
      got = HandlersE.get_packet_definitions()

      expected = [
        %PacketDefinition{
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
      got = HandlersF.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: nil,
          fields: [
            %FieldDefinition{
              description: nil,
              name: :first_field,
              type: :string,
              opts: []
            }
          ],
          name: :packet_no_desc_with_fields_no_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "attribute documentation" do
      got = HandlersG.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: nil,
          fields: [
            %FieldDefinition{
              description: "Description attribute",
              name: :first_field,
              type: :integer,
              opts: []
            }
          ],
          name: :packet_no_desc_with_fields_desc_attr,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentation option" do
      got = HandlersH.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: nil,
          fields: [
            %FieldDefinition{
              description: "Description in options",
              name: :first_field,
              type: :string,
              opts: []
            }
          ],
          name: :packet_no_desc_with_fields_desc_opts,
          tags: []
        }
      ]

      assert expected == got
    end

    test "multiline documentation" do
      got = HandlersI.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: nil,
          fields: [
            %FieldDefinition{
              description: "Some\n\nmulti lines\n\ndesc",
              name: :first_field,
              type: :string,
              opts: []
            }
          ],
          name: :packet_no_desc_with_fields_multi_line_desc,
          tags: []
        }
      ]

      assert expected == got
    end

    test "documentations previously set" do
      got = HandlersJ.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: nil,
          fields: [
            %FieldDefinition{
              description: "Here is the real description",
              name: :first_field,
              type: :string,
              opts: []
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
      got = HandlersK.get_packet_definitions()

      expected = [
        %PacketDefinition{
          description: nil,
          fields: [],
          name: :useless_packet_no_desc,
          tags: [:useless_packet]
        }
      ]

      assert expected == got
    end

    test "documentation" do
      got = HandlersL.get_packet_definitions()

      expected = [
        %PacketDefinition{
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
      got = HandlersZ.get_packet_definitions()

      expected = [
        %PacketDefinition{
          name: :mov,
          description: nil,
          tags: [],
          fields: [
            %FieldDefinition{
              description: nil,
              name: :x,
              type: :integer,
              opts: []
            },
            %FieldDefinition{
              description: nil,
              name: :y,
              type: :integer,
              opts: []
            }
          ]
        },
        %PacketDefinition{
          name: :login,
          description: "Simple login packet",
          tags: [],
          fields: [
            %FieldDefinition{
              description: nil,
              name: :username,
              type: :string,
              opts: []
            },
            %FieldDefinition{
              description: nil,
              name: :password,
              type: :string,
              opts: []
            },
            %FieldDefinition{
              description: "Seems to be always `NONE`",
              name: :unknown1,
              type: :string,
              opts: []
            },
            %FieldDefinition{
              description: "Unknown too. Maybe a random number ?",
              name: :unknown2,
              type: :integer,
              opts: []
            }
          ]
        },
        %PacketDefinition{
          name: :useless_packet,
          description: "Don't know what is this packet",
          tags: [:useless_packet],
          fields: []
        }
      ]

      assert expected == got
    end
  end
end
