module Main where

import Concur.Backend.Menu (Wid, button, delay, orr, runWid)
import Concur.Types (effect)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Array as Array
import Data.Function (($))
import Data.Functor (map, ($>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console as Console

main :: Effect Unit
main = runWid menu

-- Little menu UI example
menu :: Wid Int
menu = do
  let b i = button (show i) $> i
  i <- orr (map b (Array.range 0 5))
  effect $ Console.log $ "PICKED: " <> show i
  effect $ Console.log "THINKING...."
  delay 2000
  effect $ Console.log "DONE THINKING.... FORCING 3 INSTEAD..."
  i <- pure 3
  mOk <- orr
    [ orr
      [ do button ("Confirm you are picking " <> show i <> ", OK?") $> Just true
      , do button "Cancel" $> Just false
      , delay 5000 $> Nothing
      ]
    , orr
      [ do button "Cancel 2" $> Just false
      ]
    ]
  case mOk of
    Nothing -> do
      effect $ Console.log "You took too long. Going back."
      menu
    Just ok -> do
      if ok then pure i else do
        effect $ Console.log "CANCELLED. RESTARTING."
        menu
