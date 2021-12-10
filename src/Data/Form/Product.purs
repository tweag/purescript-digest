module Data.Form.Product
  ( ProductForm(..)
  , ProductContext
  , product
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.Form
  ( class FormContext
  , class IsForm
  , Form
  , ctx_current
  , ctx_initial
  , ctx_load
  , ctx_output
  , ctx_update
  , form
  , fromForm
  , toForm
  )
import Data.Form.Result (Result, ignore)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data ProductContext c1 c2 = ProductContext c1 c2

newtype ProductForm c1 c2 e a = ProductForm (Form (ProductContext c1 c2) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

product
  :: forall f1 f2 c1 c2 i1 i2 o1 o2 e1 e2 a b e
   . FormContext c1 i1 o1
  => FormContext c2 i2 o2
  => IsForm f1 c1 e1 a
  => IsForm f2 c2 e2 b
  => f1 e1 a
  -> f2 e2 b
  -> ProductForm (f1 e1 a) (f2 e2 b) e (a /\ b)
product f1 f2 = form (ProductContext f1 f2) ignore

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericProductForm :: Generic (ProductForm c1 c2 e a) _

derive instance newtypeProductForm :: Newtype (ProductForm c1 c2 e a) _

derive instance eqProductForm ::
  ( Eq c1
  , Eq c2
  , Eq e
  , Eq a
  ) =>
  Eq (ProductForm c1 c2 e a)

derive instance functorProductForm :: Functor (ProductForm c1 c2 e)

derive newtype instance invariantProductForm :: Invariant (ProductForm c1 c2 e)

derive newtype instance bifunctorProductForm :: Bifunctor (ProductForm c1 c2)

derive newtype instance foldableProductForm :: Foldable (ProductForm c1 c2 e)

derive newtype instance bifoldableProductForm :: Bifoldable (ProductForm c1 c2)

instance arbitraryNewtypeProductContext ::
  ( Arbitrary c1
  , Arbitrary c2
  , Arbitrary i1
  , Arbitrary i2
  , Arbitrary e1
  , Arbitrary e2
  , Arbitrary a
  , Arbitrary b
  , Coarbitrary o1
  , Coarbitrary o2
  , FormContext c1 i1 o1
  , FormContext c2 i2 o2
  , IsForm (f1 c1) c1 e1 a
  , IsForm (f2 c2) c2 e2 b
  ) =>
  Arbitrary (ProductContext (f1 c1 e1 a) (f2 c2 e2 b)) where
  arbitrary = map (bimap fromForm fromForm)
    $ ProductContext <$> arbitrary <*> arbitrary

derive instance eqProductContext :: (Eq c1, Eq c2) => Eq (ProductContext c1 c2)
derive instance ordProductContext ::
  ( Ord c1
  , Ord c2
  ) =>
  Ord (ProductContext c1 c2)

derive instance functorProductContext :: Functor (ProductContext c1)
instance bifunctorProductContext :: Bifunctor ProductContext where
  bimap f g (ProductContext c1 c2) = ProductContext (f c1) (g c2)

instance showProductContext :: (Show c1, Show c2) => Show (ProductContext c1 c2) where
  show (ProductContext c1 c2) =
    "(ProductContext " <> show c1 <> " " <> show c2 <> ")"

instance formContextProduct ::
  ( FormContext c1 i1 o1
  , FormContext c2 i2 o2
  , IsForm f1 c1 e1 a
  , IsForm f2 c2 e2 b
  ) =>
  FormContext
    (ProductContext (f1 e1 a) (f2 e2 b))
    (i1 /\ i2)
    (Result (e1 \/ e2) (a /\ b)) where
  ctx_current (ProductContext f1 f2) =
    ctx_current (toForm f1) /\ ctx_current (toForm f2)
  ctx_initial (ProductContext f1 f2) =
    ctx_initial (toForm f1) /\ ctx_initial (toForm f2)
  ctx_load (i1 /\ i2) (ProductContext f1 f2) =
    ProductContext
      (fromForm $ ctx_load i1 $ toForm f1)
      (fromForm $ ctx_load i2 $ toForm f2)
  ctx_output (ProductContext f1 f2) =
    Tuple
      <$> lmap Left (ctx_output (toForm f1))
      <*> lmap Right (ctx_output (toForm f2))
  ctx_update (i1 /\ i2) (ProductContext f1 f2) =
    ProductContext
      (fromForm $ ctx_update i1 $ toForm f1)
      (fromForm $ ctx_update i2 $ toForm f2)
