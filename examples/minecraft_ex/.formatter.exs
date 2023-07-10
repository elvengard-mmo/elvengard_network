# Used by "mix format"
locals_without_parens = [
  field: 2,
  field: 3,
  packet: 1,
  packet: 2,
  packet: 3
]

[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  locals_without_parens: locals_without_parens
]
