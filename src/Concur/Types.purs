module Concur.Types where

import Control.Applicative (class Applicative, pure, (*>))
import Control.Apply (class Apply, apply)
import Control.Bind (class Bind, bind, discard, join, (>>=))
import Control.Category (identity)
import Control.Monad (class Monad, unless, when)
import Control.Monad.Free (Free)
import Control.Monad.Free as F
import Control.Monad.Rec.Class (class MonadRec)
import Control.MonadFix (mfix)
import Data.Array ((!!))
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Boolean (otherwise)
import Data.CommutativeRing ((+))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Function (const, (#), ($), (<<<))
import Data.Functor (class Functor, map, ($>), (<$>))
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Newtype as N
import Data.Number (isNaN)
import Data.Semigroup (class Semigroup, append, (<>))
import Data.Show (class Show, show)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.ReadLine as Readline
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | Callback -> Effect Canceler (returns the unused effect)
-- | Canceling will *always* have some leftover effect, else it would have ended already
-- | TODO: Have a way to check if the callback is finished (i.e. will never be called again)
-- |       One option is to have a cb = (Either partResult a -> Effect Unit)
newtype Callback a = Callback (Callback' a)
type Callback' a = (a -> Effect Unit) -> Effect (Effect (Callback a))

mkCallback :: forall a. Callback' a -> Callback a
mkCallback = Callback

runCallback :: forall a. Callback a -> Callback' a
runCallback (Callback f) = f

instance functorCallback :: Functor Callback where
  map f g = mkCallback \cb -> map (map f) <$> runCallback g (cb <<< f)

-- | The Applicative instance for callbacks behaves as follows -
-- | Every time either of cf or cv return values, the callback will be updated with (f v)
-- | This can happen multiple times
instance applyCallback :: Apply Callback where
  apply cf cv = mkCallback \cb -> do
    fref <- Ref.new Nothing
    vref <- Ref.new Nothing
    cancelerF <- runCallback cf \f -> do
      Ref.write (Just f) fref
      mv <- Ref.read vref
      case mv of
        Just v -> cb (f v)
        _ -> pure unit
    cancelerV <- runCallback cv \v -> do
      Ref.write (Just v) vref
      mv <- Ref.read fref
      case mv of
        Just f -> cb (f v)
        _ -> pure unit
    pure do
      cf' <- cancelerF
      cv' <- cancelerV
      pure (apply cf' cv')

-- | A callback that will never be resolved
never :: forall a. Callback a
never = mkCallback \_cb -> pure (pure never)

-- | This is tricky. The current implementation can be surprising to the calling code
-- | as we immediately resolve the callback, even before the canceler is returned.
-- | Also note that when you cancel this callback, the continuation will be `never`
instance applicativeCallback :: Applicative Callback where
  pure a = mkCallback \cb -> cb a $> pure never

-- NOTE: We currently have no monadic instance for callbacks
-- Remember: The monadic instance *must* agree with the applicative instance

-- A Widget is basically a callback that returns a view or a return value
newtype Widget v a = Widget (Callback (Either v a))
derive instance functorWidget :: Functor (Widget v)
instance newtypeWidget :: Newtype (Widget v a) (Callback (Either v a))

unWid :: forall v a. Widget v a -> Callback (Either v a)
unWid (Widget w) = w

runWidget :: forall v a. Widget v a -> Callback' (Either v a)
runWidget (Widget (Callback e)) = e

mkWidget :: forall v a. Callback' (Either v a) -> Widget v a
mkWidget e = Widget (Callback e)

instance applyWidget :: Apply (Widget v) where
  apply wf wv = do
    f <- wf
    v <- wv
    pure (f v)

instance applicativeWidget :: Applicative (Widget v) where
  pure a = Widget (pure (Right a))

instance bindWidget :: Bind (Widget v) where
  bind m f = mkWidget \cb -> do
    -- CancelerRef starts out as a canceler for A, then becomes canceler for B
    cancelerRef <- mfix \cancelerRef -> do
      cancelerA <- runWidget m \res -> do
        case res of
          Left v -> cb (Left v)
          Right a -> do
            -- After A has been resolved, the canceler just becomes a canceler for B
            -- TODO: Should cancelerA also be cancelled here?
            --   Depends on what the ideal API contract is. INVESTIGATE.
            cancelerB <- runWidget (f a) cb
            Ref.write cancelerB (cancelerRef unit)

      -- The initial canceler just cancels A, and then binds the remaining widget with B
      Ref.new do
        c <- cancelerA
        pure (unWid (bind (Widget c) f))

    -- The returned canceler just reads the canceler ref and runs it
    pure (join (Ref.read cancelerRef))

-- | ORRing two widgets
orr :: forall v a. Monoid v => Widget v a -> Widget v a -> Widget v a
orr wa wb = mkWidget \cb -> do
  viewsRef <- Ref.new {viewA: mempty, viewB: mempty}
  {cancelerA, cancelerB} <- mfix \ cancelers -> do
    cancelerA <- runWidget wa \res -> do
      case res of
        Left va -> do
          views <- Ref.read viewsRef
          cb (Left (va <> views.viewB))
          Ref.write {viewA: va, viewB: views.viewB} viewsRef
        Right a -> do
          cb (Right a)
          restB <- (cancelers unit).cancelerB
          pure unit
    cancelerB <- runWidget wb \res -> do
      case res of
        Left vb -> do
          views <- Ref.read viewsRef
          cb (Left (views.viewA <> vb))
          Ref.write {viewA: views.viewA, viewB: vb} viewsRef
        Right b -> do
          cb (Right b)
          restA <- cancelerA
          pure unit
    pure {cancelerA, cancelerB}
  pure do
    restA <- cancelerA
    restB <- cancelerB
    pure (unWid (orr (Widget restA) (Widget restB)))



-- Simple menu views
-- An option has the option text, and a callback for when selected
data Option = Option String (Effect Unit)
-- Simple menu view, is a collection of options
-- There is no user input apart from choosing one option
-- Arrays already have a monoid instance
type View = Array Option
-- Widget that uses this view type
type Wid a = Widget View a

-- Sample wid
button :: String -> Wid Unit
button s = mkWidget \cb -> do
  let handler = cb (Right unit)
  cb (Left [Option s handler])
  pure (pure (unWid (button s)))

-- Make a little menu UI
menu :: Wid Int
menu = do
  let b i = button (show i) $> i
  i <- foldl orr (b 0) (map b (Array.range 1 10))
  ok <- orr
    do button ("Confirm you are picking " <> show i <> ", OK?") $> true
    do button "Cancel" $> false
  if ok then pure i else menu

-- A Widget driver
runWid :: forall a. Show a => Wid a -> Effect Unit
runWid w = do
  interfaceRef <- Ref.new Nothing
  _ <- runWidget w \res -> case res of
    Left v -> do
      prevInterface <- Ref.read interfaceRef
      prevInterface # maybe (pure unit) Readline.close
      interface <- handleOptions v
      Ref.write (Just interface) interfaceRef
    Right a -> Console.log ("YOU PICKED " <> show a)
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
