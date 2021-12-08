{ name = "forms"
, dependencies =
  [ "aff"
  , "effect"
  , "gen"
  , "integers"
  , "nonempty"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "spec"
  , "spec-discovery"
  , "spec-quickcheck"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
