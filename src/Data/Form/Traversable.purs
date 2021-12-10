module Data.Form.Traversable
  ( TraversableForm(..)
  , TraversableContext
  , traversable
  , array
  , map_
  , list
  ) where

import Prelude

import Control.Alternative (class Plus)
import Control.Plus (empty)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Form
  ( class ArbitraryFormContext
  , class FormContext
  , class IsForm
  , Form
  , arbitraryFormContext
  , coarbitraryFormContext
  , ctx_current
  , ctx_initial
  , ctx_load
  , extractContext
  , form
  , genBlank
  , peekResult
  )
import Data.Form.Result (ignore)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Test.QuickCheck (class Arbitrary, class Coarbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data TraversableContext t ctx = TraversableContext ctx (t ctx) (t ctx)

newtype TraversableForm t ctx e a =
  TraversableForm (Form (TraversableContext t ctx) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

traversable
  :: forall t f ctx i o e' a e
   . Plus t
  => Traversable t
  => FormContext ctx i o
  => IsForm f ctx e' a
  => f e' a
  -> TraversableForm t ctx e (t a)
traversable f =
  form (TraversableContext (extractContext f) empty empty) validate
  where
  validate = ignore <<< traverse (flip peekResult f)

array
  :: forall f ctx i o e' a e
   . FormContext ctx i o
  => IsForm f ctx e' a
  => f e' a
  -> TraversableForm Array ctx e (Array a)
array = traversable

map_
  :: forall f k ctx i o e' a e
   . FormContext ctx i o
  => Ord k
  => IsForm f ctx e' a
  => f e' a
  -> TraversableForm (Map k) ctx e ((Map k) a)
map_ = traversable

list
  :: forall f ctx i o e' a e
   . FormContext ctx i o
  => IsForm f ctx e' a
  => f e' a
  -> TraversableForm List ctx e (List a)
list = traversable

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericTraversableForm :: Generic (TraversableForm c1 c2 e a) _

derive instance newtypeTraversableForm :: Newtype (TraversableForm c1 c2 e a) _

derive instance eqTraversableForm ::
  ( Eq1 t
  , Eq ctx
  , Eq e
  , Eq a
  ) =>
  Eq (TraversableForm t ctx e a)

derive instance functorTraversableForm :: Functor (TraversableForm c1 c2 e)

derive newtype instance invariantTraversableForm ::
  Invariant (TraversableForm c1 c2 e)

derive newtype instance bifunctorTraversableForm ::
  Bifunctor (TraversableForm c1 c2)

derive newtype instance foldableTraversableForm ::
  Foldable (TraversableForm c1 c2 e)

derive newtype instance bifoldableTraversableForm ::
  Bifoldable (TraversableForm c1 c2)

derive newtype instance arbitraryTraversableForm ::
  ( ArbitraryFormContext ctx i o
  , Plus t
  , Traversable t
  , Arbitrary (t ctx)
  , Arbitrary (t i)
  , Arbitrary e
  , Arbitrary a
  , Coarbitrary (t ctx)
  ) =>
  Arbitrary (TraversableForm t ctx e a)

derive newtype instance coarbitraryTraversableForm ::
  ( FormContext (TraversableContext t ctx) i o
  , Coarbitrary o
  , Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (TraversableForm t ctx e a)

derive instance eqTraversableContext ::
  ( Eq1 t
  , Eq ctx
  ) =>
  Eq (TraversableContext t ctx)

derive instance ordTraversableContext ::
  ( Ord1 t
  , Ord ctx
  ) =>
  Ord (TraversableContext t ctx)

derive instance functorTraversableContext ::
  Functor t =>
  Functor (TraversableContext t)

instance foldableFoldableContext ::
  Foldable t =>
  Foldable (TraversableContext t) where
  foldMap f (TraversableContext prototype initial current) =
    f prototype <> foldMap f initial <> foldMap f current
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance traversableTraversableContext ::
  Traversable t =>
  Traversable (TraversableContext t) where
  traverse f (TraversableContext prototype initial current) =
    TraversableContext
      <$> f prototype
      <*> (traverse f initial)
      <*> (traverse f current)
  sequence = sequenceDefault

instance showTraversableContext ::
  ( Show ctx
  , Show (t ctx)
  ) =>
  Show (TraversableContext t ctx) where
  show (TraversableContext prototype initial ctx) =
    "(TraversableContext "
      <> show prototype
      <> " "
      <> show initial
      <> " "
      <> show ctx
      <> ")"

instance formContextTraversable ::
  ( FormContext ctx i o
  , Traversable t
  ) =>
  FormContext (TraversableContext t ctx) (t i) (t ctx) where
  ctx_current (TraversableContext _ _ tcurrent) = ctx_current <$> tcurrent
  ctx_initial (TraversableContext _ tinitial _) = ctx_initial <$> tinitial
  ctx_load ti (TraversableContext prototype _ _) =
    TraversableContext
      prototype
      (flip ctx_load prototype <$> ti)
      (flip ctx_load prototype <$> ti)
  ctx_output (TraversableContext _ _ tcurrent) = tcurrent
  ctx_update ti (TraversableContext prototype tinitial _) =
    TraversableContext
      prototype
      tinitial
      (flip ctx_load prototype <$> ti)

instance arbitraryFormContextTraversable ::
  ( ArbitraryFormContext ctx i o
  , Plus t
  , Traversable t
  ) =>
  ArbitraryFormContext (TraversableContext t ctx) (t i) (t ctx) where
  genBlank = TraversableContext <$> genBlank <*> pure empty <*> pure empty

instance arbitraryTraversableContext ::
  ( ArbitraryFormContext ctx i o
  , Plus t
  , Traversable t
  , Arbitrary (t ctx)
  , Arbitrary (t i)
  ) =>
  Arbitrary (TraversableContext t ctx) where
  arbitrary = arbitraryFormContext

instance coarbitraryTraversableContext ::
  ( FormContext (TraversableContext t ctx) i o
  , Coarbitrary o
  ) =>
  Coarbitrary (TraversableContext t ctx) where
  coarbitrary = coarbitraryFormContext

