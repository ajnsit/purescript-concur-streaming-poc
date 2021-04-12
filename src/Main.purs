module Main where

import Concur.Types
import Prelude

import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (show)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = runWid menu
