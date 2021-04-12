{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "free"
  , "node-readline"
  , "psci-support"
  , "refs"
  , "arrays"
  , "bifunctors"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "safe-coerce"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
