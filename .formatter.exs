# Used by "mix format"

inputs =
  Enum.flat_map(
    ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
    &Path.wildcard(&1, match_dot: true)
  ) -- ["lib/elven_gard/network/uuid.ex"]

locals_without_parens = [
  field: 2,
  field: 3,
  packet: 1,
  packet: 2,
  packet: 3,
  import_packets: 1
]

[
  inputs: inputs,
  locals_without_parens: locals_without_parens,
  export: [locals_without_parens: locals_without_parens]
]
