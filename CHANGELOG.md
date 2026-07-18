# Changelog

## Unreleased

### Breaking changes

- Replace the Ranch-specific `Endpoint.Protocol` API with the
  transport-independent `SocketHandler` behaviour.
- Replace the socket's Ranch-specific transport fields with a `Socket.Adapter`
  and its opaque state. `Socket.new/3` now receives an adapter, adapter options,
  and a network codec.
- Replace the old endpoint `:transport_opts`, `:protocol`, and `:protocol_opts`
  configuration with `:adapter`, `:adapter_options`, `:socket_handler`,
  `:transport`, and `:transport_options`.
- Require custom endpoint adapters to implement `Endpoint.Adapter.child_spec/3`.
- Normalize transport lifecycle reasons passed to `SocketHandler.handle_halt/2`
  to `:closed`, `:timeout`, or `{:error, reason}`. The callback must return
  `{:ok, socket}`.

### Added

- Add a Thousand Island endpoint adapter.
- Add transport-independent endpoint connection and packet-processing runtimes.
- Add `Socket.setopts/2` and `Socket.close/1`.
- Support TCP and TLS with both Ranch and Thousand Island.

### Changed

- Make Ranch and Thousand Island optional dependencies selected explicitly per
  endpoint.
- Resolve the socket handler, network codec, and packet handler once when an
  endpoint starts, then pass them explicitly to `Endpoint.Adapter.child_spec/3`.
- Run halt cleanup when either listener implementation shuts down active
  connections.

## 0.1.1

Add `.formatter.exs` file to release for `import_deps`

## 0.1.0

Pre-release, this is not a production ready release !
