{ name = "forms"
, dependencies =
  [ "aff"
  , "effect"
  , "prelude"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
