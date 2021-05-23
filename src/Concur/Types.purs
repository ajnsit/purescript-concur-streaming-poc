module Concur.Types where

import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply)
import Control.Bind (class Bind, bind, discard, join, (>>=))
import Control.Monad (class Monad, ap)
import Control.MonadFix (mfix)
import Data.Array ((:))
import Data.Array as Array
import Data.CommutativeRing ((+))
import Data.Function (flip, (#), ($), (<<<))
import Data.Functor (class Functor, map, void, ($>), (<$), (<$>))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (sequence, sequence_, traverse, traverse_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Ref as Ref
import Foreign.Object as Object
import Safe.Coerce (coerce)


-- | Callback -> Effect Canceler (returns the unused effect)
-- | Canceling will *always* have some leftover effect, else it would have ended already
-- | TODO: Have a way to check if the callback is finished (i.e. will never be called again)
-- |       One option is to have a cb = (Either partResult a -> Effect Unit)
type Callback v a = (Result v a -> Effect Unit) -> Effect (WidgetHandle v a)

mapCallback :: forall v a b. (a -> b) -> Callback v a -> Callback v b
mapCallback f g = \cb -> map f <$> g (cb <<< map f)

newtype WidgetHandle v a = WidgetHandle (Effect (Callback v a))

derive instance newtypeWidgetHandle :: Newtype (WidgetHandle v a) _

instance functorWidgetHandle :: Functor (WidgetHandle v) where
  map f (WidgetHandle e) = WidgetHandle (map (mapCallback f) e)

widgetHandleFromCanceler :: forall v a. Effect (Callback v a) -> WidgetHandle v a
widgetHandleFromCanceler = WidgetHandle

widgetHandleFromSimpleCanceler :: forall v a. Effect Unit -> WidgetHandle v a
widgetHandleFromSimpleCanceler e = WidgetHandle (never <$ e)

-- | A callback that will never be resolved
never :: forall v a. Callback v a
never = \_cb -> pure (widgetHandleFromCanceler (pure never))

-- An Array context with a hole
type ZipList a = {left :: Array a, right :: Array a}

-- A Widget is basically a callback that returns a view or a return value
data Result v a = View v | Eff (Effect Unit) | Result {result :: a, remaining :: Unit -> ZipList (RemainingWidget v a)}

derive instance functorResult :: Functor (Result v)
newtype Widget v a = Widget (Callback v a)
instance functorWidget :: Functor (Widget v) where
  map f (Widget w) = Widget (mapCallback f w)
derive instance newtypeWidget :: Newtype (Widget v a) _

-----------------------
-- LIFECYCLE METHODS --

-- | Cancel a callback, returns the remaining callback
cancel' :: forall v a. WidgetHandle v a -> Effect (Callback v a)
cancel' = coerce

-- | Cancel a callback, returns the remaining widget
cancel :: forall v a. WidgetHandle v a -> Effect (Widget v a)
cancel = coerce

-- | Cancel an array of callbacks, returns the array of remaining widgets
cancelMany :: forall v a. Array (WidgetHandle v a) -> Effect (Array (Widget v a))
cancelMany = sequence <<< coerce

-----------------------

mkResult :: forall v a. a -> Result v a
mkResult result = Result {result, remaining: \_ -> {left: [], right: []}}

unWid :: forall v a. Widget v a -> Callback v a
unWid = coerce

runWidget :: forall v a. Widget v a -> Callback v a
runWidget = coerce

-- | Create a widget from a callback
mkWidget :: forall v a. Callback v a -> Widget v a
mkWidget = Widget

instance applyWidget :: Apply (Widget v) where
  apply = ap

instance applicativeWidget :: Applicative (Widget v) where
  pure a = mkWidget \cb -> cb (mkResult a) $> widgetHandleFromCanceler (pure never)

instance bindWidget :: Bind (Widget v) where
  bind m f = mkWidget \cb -> do
    actionsRef <- Ref.new (Just [])
    -- CancelerRef starts out as a canceler for A, then becomes canceler for B
    cancelerRef <- mfix \cancelerRef -> do
      widgetHandleA <- runWidget m \res -> do
        let action = case res of
              Eff e -> e
              View v -> cb (View v)
              Result a -> do
                -- After A has been resolved, the canceler just becomes a canceler for B
                -- TODO: Should widgetHandleA also be cancelled here?
                --   Depends on what the ideal API contract is. INVESTIGATE.
                -- NOTE: We can discard the remainder from A, since it's already moved on.
                widgetHandleB <- runWidget (f a.result) cb
                Ref.write (cancel' widgetHandleB) (cancelerRef unit)
        Ref.read actionsRef >>= maybe action (flip Ref.write actionsRef <<< Just <<< (action : _))

      -- The initial canceler just cancels A, and then binds the remaining widget with B
      Ref.new do
        w <- cancel widgetHandleA
        pure $ unWid $ bind w f

    -- Now that we have the cancelers, we can run any pending effects
    Ref.read actionsRef >>=
      maybe (pure unit) \actions -> do
        Ref.write Nothing actionsRef
        sequence_ actions

    -- The returned canceler just reads the canceler ref and runs it
    pure $ widgetHandleFromCanceler (join (Ref.read cancelerRef))

instance monadWidget :: Monad (Widget v)

type SiblingLifecycleHandler v a = ZipList (RemainingWidget v a) -> Effect (ZipList (RemainingWidget v a))

-- | A stopped Widget which is not populated, or a handle to an already running and populated widget
data RemainingWidget v a = RunningWidget (WidgetHandle v a) | StoppedWidget (Widget v a)
derive instance functorRemainingWidget :: Functor (RemainingWidget v)

cancelRemainingWidget :: forall v a. RemainingWidget v a -> Effect (Widget v a)
cancelRemainingWidget = case _ of
  StoppedWidget w -> pure w
  RunningWidget c -> cancel c

cancelManyRemainingWidget :: forall v a. Array (RemainingWidget v a) -> Effect (Array (Widget v a))
cancelManyRemainingWidget arr = traverse cancelRemainingWidget arr

-- Default behavior is to cancel all the siblingWids
defaultSiblingLifecycleHandler :: forall v a. SiblingLifecycleHandler v a
defaultSiblingLifecycleHandler siblingLifecycles = do
  left <- map StoppedWidget <$> cancelManyRemainingWidget siblingLifecycles.left
  right <- map StoppedWidget <$> cancelManyRemainingWidget siblingLifecycles.right
  pure {left, right}

-- | Orr multiple widgets together, canceling any sibling widgets when a widget returns
cancelMOrr :: forall v a. (Object.Object v -> v) -> (v -> v -> Maybe v) -> Array (RemainingWidget v a) -> (Widget v a)
cancelMOrr = mOrr defaultSiblingLifecycleHandler

-- | Orr multiple widgets together
mOrr :: forall v a. SiblingLifecycleHandler v a -> (Object.Object v -> v) -> (v -> v -> Maybe v) -> Array (RemainingWidget v a) -> Widget v a
mOrr handleSiblingLifecycle mergeView stepView = go
  where
  go :: Array (RemainingWidget v a) -> (Widget v a)
  go widgets = mkWidget \cb -> do
    viewsRef <- Ref.new Object.empty

    -- TODO: Need to protect these views from race conditions
    let updateView label newView = do
          views <- Ref.read viewsRef
          Object.lookup label views
            # maybe (Just newView) (flip stepView newView)
            # traverse \v' -> do
                let views' = Object.insert label v' views
                Ref.write views' viewsRef
                pure views'

    actionsRef <- Ref.new (Just [])
    cancelers <- mfix \cancelersThunk ->
      forWithIndex widgets \indexWidget -> case _ of
        RunningWidget c -> pure (RunningWidget c)
        StoppedWidget wid -> RunningWidget <$> runWidget wid \res -> do
          let action = case res of
                Eff e -> e
                View v -> do
                  mv' <- updateView (show indexWidget) v
                  mv' # traverse_ (cb <<< View <<< mergeView)
                Result a -> do
                  -- TODO: Notice that this also calls the canceler for the finishing widget
                  -- Not sure if that's the correct approach. Depends on the API contract.
                  -- HACK: Use `pure` instead of `let` to ensure cancelers computation is not floated up out of the action
                  cancelers <- pure (cancelersThunk unit)
                  let leftCancelers = Array.slice 0 indexWidget cancelers
                  let rightCancelers = Array.slice (indexWidget+1) (Array.length cancelers) cancelers
                  siblingWids <- handleSiblingLifecycle { left: leftCancelers, right: rightCancelers }
                  cb (Result { result: a.result
                             , remaining: \_ ->
                                 let rem = a.remaining unit
                                 in { left: siblingWids.left <> rem.left, right: rem.right <> siblingWids.right }
                             })
          mactions <- Ref.read actionsRef
          mactions # maybe action \actions -> case res of
            -- Specifically for the view case, we only report the view after
            -- the widget has finished initialising
            View v -> void $ updateView (show indexWidget) v
            _ -> Ref.write (Just (action : actions)) actionsRef

    -- Now that we have the cancelers, we can run any pending actions
    Ref.read actionsRef >>=
      maybe (pure unit) \effects -> do
        Ref.write Nothing actionsRef
        sequence_ effects

    -- And report the view
    view <- Ref.read viewsRef
    cb (View (mergeView view))

    pure $ widgetHandleFromCanceler $ map (unWid <<< go) $ map StoppedWidget <$> cancelManyRemainingWidget cancelers

    -- TODO: We eventually want this following formulation to work. This does compile, but doesn't cancel things which messes up the UI.
    -- When canceling the top level ORR widget, we want to not have to immediately cancel all the ORR'd widgets, and still have the UI be consistent
    -- pure $ widgetHandleFromCanceler $ pure $ unWid $ go cancelers



-- | Run an effect inside a widget
effect :: forall v a. Effect a -> Widget v a
effect e = mkWidget \cb -> do
  e >>= (cb <<< mkResult)
  pure $ widgetHandleFromCanceler $ pure never

-- A simple async effect, takes in a callback and returns a canceler
type Async a = (a -> Effect Unit) -> Effect (Effect Unit)

-- | Run an async action inside a widget
affect :: forall v a. Async a -> Widget v a
affect callback = mkWidget \cb -> do
  canceler <- callback (cb <<< mkResult)
  pure (widgetHandleFromSimpleCanceler canceler)

-- | Make an object from an array. With element indices, converted to string, as the keys.
-- mkObject :: forall a. Array a -> Object a
-- mkObject = Object.fromFoldable <<< mapWithIndex (Tuple <<< show)

-- A common pattern
-- ORR a bunch of widgets, after one returns process it, and then continue with the modified widget
-- morr :: forall v a r. Monoid v => (a -> Widget v (Either a r)) -> Array a -> Widget v r
-- morr f arr = mkWidget \cb -> do
--   canceler <- runWidget (multiorr (map f arr)) \res -> do
--     case res of
--       View v -> cb (View v)
--       Result r -> do
--         let w' = f r.result
--         let rem = r.remaining
--         let marr = Array.updateAt

-- TODO
-- Add nesting widgets
  -- Also then use ZipTree instead of ZipList.
-- Static views
-- Effects
-- Async
