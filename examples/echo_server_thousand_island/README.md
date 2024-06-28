# EchoServer

This is a pretty simple Echo server built with ElvenGard.Network.

## Usage

    > cd examples/echo_server
    > mix deps.get
    > mix run --no-halt

By default, the server listens on port `3333`. It is possible to change this port in the configuration file (`config/config.exs`).

Here is an example of use :

    # On the first terminal
    > iex -S mix
    Erlang/OTP 25 [erts-13.1.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]
    Interactive Elixir (1.14.3) - press Ctrl+C to exit (type h() ENTER for help)

    # On the second one
    > nc 127.0.0.1 3333
    hey, it's my first message
    REPLY:hey, it's my first message  < echo from the server
    how are you ?
    REPLY:how are you ?               < echo from the server
    I'm going to leave
    REPLY:I'm going to leave          < echo from the server
    [CTRL+C to leave]
    
    # Back to our first terminal
    17:35:01.636 application=echo_server [info] EchoServer started on 127.0.0.1:3333
    17:35:39.502 application=echo_server [info] New connection: e4b53371-f070-4d99-8566-b78f3bc0f953
    17:35:54.314 application=echo_server [debug] New message from e4b53371-f070-4d99-8566-b78f3bc0f953 (len: 27)
    17:36:02.607 application=echo_server [debug] New message from e4b53371-f070-4d99-8566-b78f3bc0f953 (len: 14)
    17:36:19.340 application=echo_server [debug] New message from e4b53371-f070-4d99-8566-b78f3bc0f953 (len: 19)
    17:36:20.459 application=echo_server [info] e4b53371-f070-4d99-8566-b78f3bc0f953 is now disconnected (reason: :tcp_closed)
