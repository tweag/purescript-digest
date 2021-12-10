module Data.Form.Product
  ( ProductForm(..)
  , product
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Foldable (class Foldable)
import Data.Form
  ( class Form
  , FormT(..)
  , arbitraryFn
  , arbitraryForm
  , bindResult
  , form
  , ignoreError
  )
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong ((***))
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype ProductForm f g e a = ProductForm (FormT (f /\ g) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

product
  :: forall f g i j e1 e2 e a b
   . Form f i
  => Form g j
  => f e1 a
  -> g e2 b
  -> ProductForm (f e1 a) (g e2 b) e (a /\ b)
product f g =
  form (f /\ g) validate
  where
  validate = unwrap $ Star ignoreError *** Star ignoreError

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericProductForm :: Generic (ProductForm f g e a) _

derive instance newtypeProductForm :: Newtype (ProductForm f g e a) _

derive instance eqProductForm ::
  ( Eq f
  , Eq g
  , Eq e
  , Eq a
  ) =>
  Eq (ProductForm f g e a)

derive instance functorProductForm :: Functor (ProductForm f g e)

derive newtype instance invariantProductForm ::
  Invariant (ProductForm f g e)

derive newtype instance bifunctorProductForm ::
  Bifunctor (ProductForm f g)

derive newtype instance foldableProductForm ::
  Foldable (ProductForm f g e)

derive newtype instance bifoldableProductForm ::
  Bifoldable (ProductForm f g)

derive newtype instance showProductForm ::
  ( Show f
  , Show g
  , Show e
  , Show a
  ) =>
  Show (ProductForm f g e a)

instance arbitraryProductForm ::
  ( Arbitrary t
  , Arbitrary e
  , Arbitrary (f e1 a)
  , Arbitrary (g e2 b)
  , Coarbitrary (f e1 a)
  , Coarbitrary (g e2 b)
  , Coarbitrary a
  , Coarbitrary b
  , Form f i
  , Form g j
  ) =>
  Arbitrary (ProductForm (f e1 a) (g e2 b) e t) where
  arbitrary = arbitraryForm
    $ (bindResult <$> arbitraryFn) <*> (product <$> arbitrary <*> arbitrary)

derive newtype instance coarbitraryProductForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (ProductForm f g e a)
