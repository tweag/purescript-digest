module Control.Monad.Digest.Trans where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , catchError
  , throwError
  )
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Control.Plus (class Plus, empty)
import Data.Align (class Align, class Alignable, align, nil)
import Data.Bifunctor (lmap)
import Data.Compactable (class Compactable, compact, separate)
import Data.Filterable
  ( class Filterable
  , filter
  , filterMap
  , partition
  , partitionMap
  )
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Class (class MonadEffect, liftEffect)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

-- | Type synonym for the output of a Digest cycle
type DigestResult v a = Tuple v (Maybe a)

-- | A Monad transformer which adds a "digest" capability to the base monad.
-- | A digest is a process that consumes input and produces some parsed output
-- | while also accumulating some monoidal view of the input. It is particularly
-- | useful for implementing form logic in a composable manner.
-- | It can be thought of as combining the abilities of `ReaderT`, `MaybeT`,
-- | and `WriterT`.
newtype DigestT view input m result = DigestT
  (input -> m (DigestResult view result))

-------------------------------------------------------------------------------
-- Morphisms
-------------------------------------------------------------------------------

-- | Run a computation in the `DigestT` monad.
runDigestT :: forall v i m a. DigestT v i m a -> i -> m (DigestResult v a)
runDigestT (DigestT d) = d

-- | Run a computation in the `DigestT` monad, discarding the view.
evalDigestT :: forall v i m a. Functor m => DigestT v i m a -> i -> m (Maybe a)
evalDigestT (DigestT d) i = snd <$> d i

-- | Run a computation in the `DigestT` monad, discarding the result.
execDigestT :: forall v i m a. Functor m => DigestT v i m a -> i -> m v
execDigestT (DigestT d) i = fst <$> d i

-- | Change the base monad, view and result types of a `DigestT` monad action.
mapDigestT
  :: forall v w i m1 m2 a b
   . (m1 (DigestResult v a) -> m2 (DigestResult w b))
  -> DigestT v i m1 a
  -> DigestT w i m2 b
mapDigestT f (DigestT d) = DigestT \i -> f (d i)

-- | Change the input type of a `DigestT` monad action.
withDigestT
  :: forall v i j m a
   . (j -> i)
  -> DigestT v i m a
  -> DigestT v j m a
withDigestT f (DigestT d) = DigestT \i -> d (f i)

-- | Hoist a natural transformation over the base monad to a natural
-- | transformation over a `DigestT`. This is just a more restricted version of
-- | `mapDigestT`.
hoistDigestT
  :: forall v i m1 m2. (m1 ~> m2) -> DigestT v i m1 ~> DigestT v i m2
hoistDigestT = mapDigestT

-- | Change the view type  of a `DigestT` monad action.
mapView
  :: forall v w i m. Functor m => (v -> w) -> DigestT v i m ~> DigestT w i m
mapView f = mapDigestT (map (lmap f))

-- | Change the result type  of a `DigestT` monad action.
mapResult
  :: forall v i m a b
   . Functor m
  => (Maybe a -> Maybe b)
  -> DigestT v i m a
  -> DigestT v i m b
mapResult f = mapDigestT (map (map f))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance newtypeDigestT :: Newtype (DigestT v i m a) _

derive instance functorDigestT :: Functor m => Functor (DigestT v i m)

instance applyDigestT :: (Apply m, Semigroup v) => Apply (DigestT v i m) where
  apply (DigestT f) (DigestT a) = DigestT (lift2 (lift2 (lift2 apply)) f a)

instance applicativeDigestT ::
  ( Applicative m
  , Monoid v
  ) =>
  Applicative (DigestT v i m) where
  pure a = DigestT \_ -> pure (Tuple mempty (Just a))

instance altDigestT :: Alt m => Alt (DigestT v i m) where
  alt (DigestT d) (DigestT e) = DigestT \i -> d i <|> e i

instance plusDigestT :: Plus m => Plus (DigestT v i m) where
  empty = DigestT \_ -> empty

instance alternativeDigestT ::
  ( Monoid v
  , Alternative m
  ) =>
  Alternative (DigestT v i m)

instance bindDigestT :: (Monad m, Semigroup v) => Bind (DigestT v i m) where
  bind (DigestT d) f = DigestT \i ->
    d i >>= \(Tuple v1 ma) ->
      case f <$> ma of
        Nothing -> pure (Tuple v1 Nothing)
        Just (DigestT d') -> (\(Tuple v2 b) -> Tuple (v1 <> v2) b) <$> d' i

instance monadDigestT :: (Monoid v, Monad m) => Monad (DigestT v i m)

instance monadTransDigestT :: Monoid v => MonadTrans (DigestT v i) where
  lift m = DigestT \_ -> m <#> \a -> Tuple mempty (Just a)

instance lazyDigestT :: Lazy (DigestT v i m a) where
  defer f = DigestT \i -> case f unit of DigestT d -> d i

instance monadEffectDigestT ::
  ( Monoid v
  , MonadEffect m
  ) =>
  MonadEffect (DigestT v i m) where
  liftEffect e = lift (liftEffect e)

instance monadAskDigestT :: (Monoid v, Monad m) => MonadAsk i (DigestT v i m) where
  ask = DigestT \i -> pure (Tuple mempty (Just i))

instance monadReaderDigestT ::
  ( Monoid v
  , Monad m
  ) =>
  MonadReader i (DigestT v i m) where
  local f (DigestT d) = DigestT \i -> d (f i)

instance monadTellDigestT :: (Monoid v, Monad m) => MonadTell v (DigestT v i m) where
  tell v = DigestT \_ -> pure (Tuple v (Just unit))

instance monadWriterDigestT ::
  ( Monoid v
  , Monad m
  ) =>
  MonadWriter v (DigestT v i m) where
  listen (DigestT d) = DigestT \i ->
    d i <#> \(Tuple v ma) -> Tuple v (ma <#> \a -> Tuple a v)
  pass (DigestT d) = DigestT \i ->
    d i <#> \(Tuple v ma) -> case ma of
      Nothing -> Tuple v Nothing
      Just (Tuple a f) -> Tuple (f v) (Just a)

instance monadThrowDigestT ::
  ( Monoid v
  , MonadThrow e m
  ) =>
  MonadThrow e (DigestT v i m) where
  throwError e = lift (throwError e)

instance monadErrorDigestT ::
  ( Monoid v
  , MonadError e m
  ) =>
  MonadError e (DigestT v i m) where
  catchError (DigestT d) h = DigestT \i ->
    catchError (d i) \e -> case h e of DigestT d' -> d' i

instance monadRecDigestT :: (Monoid v, MonadRec m) => MonadRec (DigestT v i m) where
  tailRecM k a = DigestT \i -> tailRecM (k' i) (Tuple mempty (Just a))
    where
    k' _ (Tuple v Nothing) = pure $ Done (Tuple v Nothing)
    k' i (Tuple v (Just a')) =
      case k a' of
        DigestT d -> d i <#> case _ of
          Tuple w Nothing -> Done (Tuple (v <> w) Nothing)
          Tuple w (Just (Loop a'')) -> Loop (Tuple (v <> w) (Just a''))
          Tuple w (Just (Done a'')) -> Done (Tuple (v <> w) (Just a''))

instance semigroupDigestT ::
  ( Apply m
  , Semigroup a
  , Semigroup v
  ) =>
  Semigroup (DigestT v i m a) where
  append = lift2 (<>)

instance monoidDigestT ::
  ( Applicative m
  , Monoid a
  , Monoid v
  ) =>
  Monoid (DigestT v i m a) where
  mempty = pure mempty

instance compactableDigestT :: Functor m => Compactable (DigestT v i m) where
  compact = mapResult compact
  separate (DigestT d) =
    let
      d' = map (map (map separate)) d
    in
      { left: DigestT (map (map (map _.left)) d')
      , right: DigestT (map (map (map _.right)) d')
      }

instance filterableDigestT :: Functor m => Filterable (DigestT v i m) where
  filter f = mapResult (filter f)
  filterMap f = mapResult (filterMap f)
  partition f (DigestT d) =
    let
      d' = map (map (map (partition f))) d
    in
      { no: DigestT (map (map (map _.no)) d')
      , yes: DigestT (map (map (map _.yes)) d')
      }
  partitionMap f (DigestT d) =
    let
      d' = map (map (map (partitionMap f))) d
    in
      { left: DigestT (map (map (map _.left)) d')
      , right: DigestT (map (map (map _.right)) d')
      }

instance alignDigestT :: (Apply m, Semigroup v) => Align (DigestT v i m) where
  align f (DigestT d) (DigestT e) =
    DigestT (lift2 (lift2 (lift2 (align f))) d e)

instance alignableDigestT ::
  ( Applicative m
  , Monoid v
  ) =>
  Alignable (DigestT v i m) where
  nil = DigestT \_ -> pure (Tuple mempty nil)
