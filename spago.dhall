{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "node-readline"
  , "psci-support"
  , "refs"
  , "arrays"
  , "foldable-traversable"
  , "foreign-object"
  , "identity"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "safe-coerce"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
