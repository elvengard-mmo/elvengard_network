# Used by "mix format"
locals_without_parens = [
  packet: 2,
  field: 2,
  field: 3,
  resolve: 1,
  ignore_packet: 1,
  defextension: 1,
  defextension: 2
]

inputs =
  Enum.flat_map(
    ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
    &Path.wildcard(&1, match_dot: true)
  ) -- ["lib/elven_gard/uuid.ex"]

[
  inputs: inputs,
  locals_without_parens: locals_without_parens,
  export: [locals_without_parens: locals_without_parens]
]
