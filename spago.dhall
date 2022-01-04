{ name = "forms"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "control"
  , "effect"
  , "filterable"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "tailrec"
  , "these"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
