defmodule ElvenGard.Protocol.TextualTest.Macros do
  @moduledoc false

  defmacro defdecoder(name, code) do
    quote do
      defmodule unquote(name) do
        alias ElvenGard.Protocol.TextualTest.SimplePacketHandler
        use ElvenGard.Protocol.Textual, model: SimplePacketHandler, separator: " "

        def handle_decode_fail(_header, _params, _model), do: {:error, :decode_fail}
        def handle_encode(data, _socket), do: data

        unquote(code)
      end
    end
  end
end

defmodule ElvenGard.Protocol.TextualTest do
  use ExUnit.Case, async: true

  import __MODULE__.Macros

  alias ElvenGard.Socket

  ## Function helpers

  def dummy_socket() do
    Socket.new(nil, nil, nil)
  end

  ## Modules helpers

  defmodule SimplePacketHandler do
    use ElvenGard.PacketHandler

    packet "test" do
      resolve fn _x, _y, _z -> :ok end
    end

    packet "header" do
      field :first, :string
      field :second, :integer
      field :third, :float
      resolve fn _x, _y, _z -> :ok end
    end
  end

  defdecoder SimpleDecoder do
    @impl ElvenGard.Protocol.Textual
    def handle_decode(data, _socket), do: data
  end

  defdecoder SimpleDecoderList do
    @impl ElvenGard.Protocol.Textual
    def handle_decode(data, _socket) do
      String.split(data, "\n", trim: true)
    end
  end

  ## Tests

  describe "Protocol raise error if:" do
    test "handle_decode/2 is not defined" do
      needle = ~r"function handle_decode/2 required by behaviour"

      assert_raise RuntimeError, needle, fn ->
        defdecoder(InvalidDecoder, do: :ok)
      end
    end

    test "handle_encode/2 is not defined" do
      needle = ~r"function handle_encode/2 required by behaviour"

      assert_raise RuntimeError, needle, fn ->
        defmodule InvalidEncoder do
          use ElvenGard.Protocol.Textual, model: SimplePacketHandler, separator: " "

          def handle_decode_fail(_header, _params, _model), do: {:error, :decode_fail}
          def handle_decode(data, _socket), do: data
        end
      end
    end
  end

  describe "Testing `Textual` behaviour (Single packet):" do
    test "empty packet" do
      got = SimpleDecoder.decode("", dummy_socket())
      expected = {:error, :decode_fail}

      assert got == expected
    end

    test "invalid header" do
      got = SimpleDecoder.decode("invalid_header some_string 1 1.1", dummy_socket())
      expected = {:error, :decode_fail}

      assert got == expected
    end

    test "less arguments count" do
      got = SimpleDecoder.decode("header", dummy_socket())
      expected = {:error, :decode_fail}

      assert got == expected
    end

    test "greater arguments count" do
      got = SimpleDecoder.decode("header some_string 1 1.1 more", dummy_socket())
      expected = {:error, :decode_fail}

      assert got == expected
    end

    test "only header" do
      got = SimpleDecoder.decode("test", dummy_socket())
      expected = {"test", %{}}

      assert got == expected
    end

    test "header and attributes" do
      got = SimpleDecoder.decode("header some_string 1 1.1", dummy_socket())
      expected = {"header", %{first: "some_string", second: 1, third: 1.1}}

      assert got == expected
    end
  end

  describe "Testing `Textual` behaviour (Multi packet):" do
    test "without packet" do
      got = SimpleDecoderList.decode("", dummy_socket())
      expected = []

      assert got == expected
    end

    test "multiple packets" do
      packets = """
      invalid_header some_string 1 1.1
      test
      header
      header some_string 1 1.1 more
      header some_string 1 1.1
      """

      got = SimpleDecoderList.decode(packets, dummy_socket())

      expected = [
        {:error, :decode_fail},
        {"test", %{}},
        {:error, :decode_fail},
        {:error, :decode_fail},
        {"header", %{first: "some_string", second: 1, third: 1.1}}
      ]

      assert got == expected
    end
  end
end
