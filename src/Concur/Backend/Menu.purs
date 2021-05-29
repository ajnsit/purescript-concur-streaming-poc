module Concur.Backend.Menu where

import Concur.Types (RemainingWidget(..), Result(..), Widget, affect, cancelMOrr, mkWidget, runWidget, unWid, widgetHandleFromCanceler)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Category (identity)
import Data.Array ((!!))
import Data.Foldable (fold)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Function (const, (#), (<<<))
import Data.Functor (map)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid)
import Data.Number (isNaN)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.ReadLine as Readline

-- Simple menu views
-- An option has the option text, and a callback for when selected
data Option = Option String (Effect Unit)
-- Simple menu view, is a collection of options
-- There is no user input apart from choosing one option
-- Arrays already have a monoid instance
type MenuView = Array Option
-- Widget that uses this view type
type Wid a = Widget MenuView a

-- Sample wid
button :: String -> Wid Unit
button s = mkWidget \cb -> do
  let handler = cb (Result {result: unit, remaining: \_ -> {left: [], right: []}})
  cb (View [Option s handler])
  pure (widgetHandleFromCanceler (pure (unWid (button s))))

-- No VDOM,
orr :: forall v a. Monoid v => Array (Widget v a) -> Widget v a
orr = cancelMOrr fold (const Just) <<< map StoppedWidget

foreign import delay_ :: Int -> Effect Unit -> Effect (Effect Unit)

delay :: forall v. Int -> Widget v Unit
delay ms = affect \cb -> delay_ ms (cb unit)

-- A Widget driver
runWid :: forall a. Show a => Wid a -> Effect Unit
runWid w = do
  interfaceRef <- Ref.new Nothing
  canceler <- runWidget w \res -> case res of
    Eff e -> e
    View v -> do
      prevInterface <- Ref.read interfaceRef
      prevInterface # maybe (pure unit) Readline.close
      interface <- handleOptions v
      Ref.write (Just interface) interfaceRef
    Result a -> Console.log ("YOU PICKED " <> show a.result)
  pure unit

handleOptions :: Array Option -> Effect Readline.Interface
handleOptions opts = do
  Console.log "Options"
  Console.log "-------"
  opts # traverseWithIndex_ \i (Option s e) -> Console.log (show i <> ") " <> s)

  interface <- Readline.createConsoleInterface Readline.noCompletion
  interface # Readline.question "Pick your choice> " \s -> do
    let mcallback = do
          i <- parseInt s
          (Option _ e) <- opts !! i
          pure e
    -- Close readline interface before proceeding with the callback
    Readline.close interface
    maybe (Console.log "INVALID INPUT") identity mcallback
  pure interface

-- Utility
foreign import unsafeParseInt :: String -> Number
parseInt :: String -> Maybe Int
parseInt s =
  let x = unsafeParseInt s
  in if isNaN x
     then Nothing
     else Just (round x)
