module Data.Form.Coproduct
  ( CoproductForm(..)
  , coproduct
  ) where

import Prelude

import Control.Comonad (extract)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
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
import Data.Profunctor.Choice ((+++))
import Data.Profunctor.Star (Star(..))
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype CoproductForm f g e a =
  CoproductForm (FormT ((g /\ f) \/ (f /\ g)) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

coproduct
  :: forall f g i j e1 e2 e a b
   . Form f i
  => Form g j
  => f e1 a
  -> g e2 b
  -> CoproductForm (f e1 a) (g e2 b) e (a \/ b)
coproduct f g =
  form (Left (g /\ f)) validate
  where
  validate =
    unwrap $ Star (ignoreError <<< extract) +++ Star (ignoreError <<< extract)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericCoproductForm :: Generic (CoproductForm f g e a) _

derive instance newtypeCoproductForm :: Newtype (CoproductForm f g e a) _

derive instance eqCoproductForm ::
  ( Eq f
  , Eq g
  , Eq e
  , Eq a
  ) =>
  Eq (CoproductForm f g e a)

derive instance functorCoproductForm :: Functor (CoproductForm f g e)

derive newtype instance invariantCoproductForm ::
  Invariant (CoproductForm f g e)

derive newtype instance bifunctorCoproductForm ::
  Bifunctor (CoproductForm f g)

derive newtype instance foldableCoproductForm ::
  Foldable (CoproductForm f g e)

derive newtype instance bifoldableCoproductForm ::
  Bifoldable (CoproductForm f g)

derive newtype instance showCoproductForm ::
  ( Show f
  , Show g
  , Show e
  , Show a
  ) =>
  Show (CoproductForm f g e a)

instance arbitraryCoproductForm ::
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
  Arbitrary (CoproductForm (f e1 a) (g e2 b) e t) where
  arbitrary = arbitraryForm
    $ (bindResult <$> arbitraryFn) <*> (coproduct <$> arbitrary <*> arbitrary)

derive newtype instance coarbitraryCoproductForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (CoproductForm f g e a)
