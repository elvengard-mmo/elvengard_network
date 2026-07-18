# Changelog

## Unreleased

- Add a Thousand Island endpoint adapter and transport-independent socket runtime.
- Make Ranch and Thousand Island optional dependencies selected explicitly per endpoint.
- Normalize connection lifecycle callbacks and require `handle_halt/2` to return `{:ok, socket}`.
- Run Ranch halt cleanup when its listener shuts down active connections.
- Resolve socket handler runtime configuration once when an endpoint starts and
  pass it explicitly to `Endpoint.Adapter.child_spec/3`.

## 0.1.1

Add `.formatter.exs` file to release for `import_deps`

## 0.1.0

Pre-release, this is not a production ready release !
