module Data.Form.Product
  ( ProductForm(..)
  , ProductContext
  , getFstForm
  , getSndForm
  , product
  , productValidate
  , setFstForm
  , setSndForm
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Form
  ( class FormContext
  , class IsForm
  , Form(..)
  , arbitraryForm
  , clear
  , current
  , currentContext
  , formValidate
  , ignoreError
  , initialContext
  , load
  , loadContext
  , loadsContext
  , save
  , updateContext
  , updatesContext
  )
import Data.Form.Result (fromEither)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (first, second, (***))
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype ProductContext f g = PC (f /\ g)

unPC :: forall f g. ProductContext f g -> f /\ g
unPC (PC t) = t

overPC
  :: forall f g h j
   . (f /\ g -> h /\ j)
  -> ProductContext f g
  -> ProductContext h j
overPC f (PC t) = PC $ f t

newtype ProductForm f g e a = ProductForm (Form (ProductContext f g) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

productValidate
  :: forall f g c1 c2 e1 e2 e a b c
   . IsForm f c1
  => IsForm g c2
  => f e1 a
  -> g e2 b
  -> (a /\ b -> Either e c)
  -> ProductForm (f e1 a) (g e2 b) e c
productValidate f g validate =
  formValidate (PC (f /\ g))
    (fromEither <<< validate <=< validateProduct <<< unPC)
  where
  validateProduct = unwrap $ Star ignoreError *** Star ignoreError

product
  :: forall f g c1 c2 e1 e2 e a b
   . IsForm f c1
  => IsForm g c2
  => f e1 a
  -> g e2 b
  -> ProductForm (f e1 a) (g e2 b) e (a /\ b)
product f g = productValidate f g Right

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericProductForm :: Generic (ProductForm f g e a) _

derive instance newtypeProductForm :: Newtype (ProductForm f g e a) _

derive instance eqProductContext :: (Eq f, Eq g) => Eq (ProductContext f g)

derive instance eqProductForm ::
  ( Eq f
  , Eq g
  , Eq e
  , Eq a
  ) =>
  Eq (ProductForm f g e a)

derive instance functorProductForm :: Functor (ProductForm f g e)

derive newtype instance invariantProductForm :: Invariant (ProductForm f g e)

derive newtype instance bifunctorProductForm :: Bifunctor (ProductForm f g)

derive newtype instance foldableProductForm :: Foldable (ProductForm f g e)

derive newtype instance bifoldableProductForm :: Bifoldable (ProductForm f g)

derive newtype instance showProductContext ::
  ( Show f
  , Show g
  ) =>
  Show (ProductContext f g)

derive newtype instance showProductForm ::
  ( Show f
  , Show g
  , Show e
  , Show a
  ) =>
  Show (ProductForm f g e a)

instance formContextProductContext ::
  ( IsForm f c1
  , IsForm g c2
  , FormContext c1 i
  , FormContext c2 j
  ) =>
  FormContext (ProductContext (f e1 a) (g e2 b)) (i /\ j) where
  clearInput = overPC $ bimap (save <<< clear) (save <<< clear)
  getInput = bimap current current <<< unPC
  setInput (i /\ j) = overPC $ (load i *** load j)

derive newtype instance arbitraryProductContext ::
  ( Arbitrary f
  , Arbitrary g
  ) =>
  Arbitrary (ProductContext f g)

instance arbitraryProductForm ::
  ( IsForm f c1
  , IsForm g c2
  , FormContext c1 i
  , FormContext c2 j
  , Arbitrary (f e1 a)
  , Arbitrary (g e2 b)
  , Arbitrary i
  , Arbitrary j
  , Arbitrary t
  , Arbitrary e
  , Coarbitrary (f e1 a)
  , Coarbitrary (g e2 b)
  , Coarbitrary a
  , Coarbitrary b
  ) =>
  Arbitrary (ProductForm (f e1 a) (g e2 b) e t) where
  arbitrary = arbitraryForm $ lcmap unPC $ uncurry productValidate

derive newtype instance coarbitraryProductForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (ProductForm f g e a)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

getFstForm
  :: forall f c g e' a' e a
   . IsForm f c
  => ProductForm (f e' a') g e a
  -> f e' a'
getFstForm pf = updateContext currentFst initialForm
  where
  currentFst = currentContext $ fst $ unPC $ currentContext pf
  initialForm = fst $ unPC $ initialContext pf

setFstForm
  :: forall f c g e' a' e a
   . IsForm f c
  => f e' a'
  -> ProductForm (f e' a') g e a
  -> ProductForm (f e' a') g e a
setFstForm f =
  updatesContext (go currentContext) <<< loadsContext (go initialContext)
  where
  go getter = overPC $ first (loadContext $ getter f)

getSndForm
  :: forall f c g e' a' e a
   . IsForm g c
  => ProductForm f (g e' a') e a
  -> g e' a'
getSndForm pf = updateContext currentSnd initialForm
  where
  currentSnd = currentContext $ snd $ unPC $ currentContext pf
  initialForm = snd $ unPC $ initialContext pf

setSndForm
  :: forall f c g e' a' e a
   . IsForm g c
  => g e' a'
  -> ProductForm f (g e' a') e a
  -> ProductForm f (g e' a') e a
setSndForm f =
  updatesContext (go currentContext) <<< loadsContext (go initialContext)
  where
  go getter = overPC $ second (loadContext $ getter f)
