module Data.Form.Traversable
  ( TraversableForm(..)
  , traversable
  ) where

import Prelude

import Control.Alternative (class Plus)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Form
  ( class Form
  , FormT(..)
  , arbitraryFn
  , arbitraryForm
  , bindResult
  , form
  , result
  )
import Data.Form.Result (ignore)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, singleton, tail)
import Data.Traversable (class Traversable, traverse)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype TraversableForm t f e a =
  TraversableForm (FormT (NonEmpty t f) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

traversable
  :: forall t f i e' e a
   . Plus t
  => Traversable t
  => Form f i
  => f e' a
  -> TraversableForm t (f e' a) e (t a)
traversable f =
  form (singleton f) validate
  where
  validate = ignore <<< traverse result <<< tail

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

derive newtype instance showCoproductForm ::
  ( Show f
  , Show (t f)
  , Show e
  , Show a
  ) =>
  Show (TraversableForm t f e a)

instance arbitraryTraversableForm ::
  ( Arbitrary u
  , Arbitrary e
  , Arbitrary (f e' a)
  , Arbitrary (t (f e' a))
  , Coarbitrary (f e' a)
  , Coarbitrary (t (f e' a))
  , Coarbitrary (t a)
  , Form f i
  , Plus t
  , Traversable t
  ) =>
  Arbitrary (TraversableForm t (f e' a) e u) where
  arbitrary = arbitraryForm
    $ (bindResult <$> arbitraryFn) <*> (traversable <$> arbitrary)

derive newtype instance coarbitraryTraversableForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (TraversableForm t ctx e a)
