module Main where

import Concur.Backend.Menu (menu, runWid)
import Data.Unit (Unit)
import Effect (Effect)

main :: Effect Unit
main = runWid menu
