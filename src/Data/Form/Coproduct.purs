module Data.Form.Coproduct
  ( CoproductForm(..)
  , CoproductContext
  , choose
  , coproduct
  , coproductValidate
  , getLeftForm
  , getRightForm
  , setLeftForm
  , setRightForm
  , toggle
  ) where

import Prelude

import Control.Comonad (extract)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.Form
  ( class FormContext
  , class IsForm
  , Form(..)
  , arbitraryForm
  , clear
  , currentContext
  , formValidate
  , ignoreError
  , initial
  , initialContext
  , load
  , loadContext
  , loadsContext
  , save
  , updateContext
  , updatesContext
  )
import Data.Form.Result (Result(..))
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((+++))
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (first, second)
import Data.Tuple (fst, snd, swap, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

type Braid f g = (g /\ f) \/ (f /\ g)

newtype CoproductContext f g = CC (Braid f g)

unCC :: forall f g. CoproductContext f g -> Braid f g
unCC (CC t) = t

formsCC :: forall f g. CoproductContext f g -> f /\ g
formsCC (CC (Left t)) = swap t
formsCC (CC (Right t)) = t

overCC
  :: forall f g h j
   . (Braid f g -> Braid h j)
  -> CoproductContext f g
  -> CoproductContext h j
overCC f (CC t) = CC $ f t

newtype CoproductForm f g e a = CoproductForm (Form (CoproductContext f g) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

coproductValidate
  :: forall f g i j e1 e2 e a b c
   . IsForm f i
  => IsForm g j
  => f e1 a
  -> g e2 b
  -> (a \/ b -> Result e c)
  -> CoproductForm (f e1 a) (g e2 b) e c
coproductValidate f g validate =
  formValidate (CC $ Left (g /\ f)) (validate <=< validateCoproduct <<< unCC)
  where
  validateCoproduct =
    unwrap $ Star (ignoreError <<< extract) +++ Star (ignoreError <<< extract)

coproduct
  :: forall f g i j e1 e2 e a b
   . IsForm f i
  => IsForm g j
  => f e1 a
  -> g e2 b
  -> CoproductForm (f e1 a) (g e2 b) e (a \/ b)
coproduct f g = coproductValidate f g Ok

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericCoproductForm :: Generic (CoproductForm f g e a) _

derive instance newtypeCoproductForm :: Newtype (CoproductForm f g e a) _

derive newtype instance eqCoproductContext ::
  ( Eq f
  , Eq g
  ) =>
  Eq (CoproductContext f g)

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

derive newtype instance showCoproductContext ::
  ( Show f
  , Show g
  ) =>
  Show (CoproductContext f g)

derive newtype instance showCoproductForm ::
  ( Show f
  , Show g
  , Show e
  , Show a
  ) =>
  Show (CoproductForm f g e a)

instance formContextCoproductContext ::
  ( IsForm f c1
  , IsForm g c2
  , FormContext c1 i
  , FormContext c2 j
  ) =>
  FormContext (CoproductContext (f e1 a) (g e2 b)) (i \/ j) where
  clearInput = CC <<< Left <<< swap <<< bimap (save <<< clear) (save <<< clear)
    <<< formsCC
  getInput = bimap (initial <<< snd) (initial <<< snd) <<< unCC
  setInput inp ctx =
    overCC (either setInputL setInputR inp) ctx
    where
    setInputL i = Left <<< map (load i) <<< either identity swap
    setInputR j = Right <<< map (load j) <<< either swap identity

derive newtype instance arbitraryCoproductContext ::
  ( Arbitrary f
  , Arbitrary g
  ) =>
  Arbitrary (CoproductContext f g)

instance arbitraryCoproductForm ::
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
  Arbitrary (CoproductForm (f e1 a) (g e2 b) e t) where
  arbitrary = arbitraryForm $ lcmap formsCC $ uncurry coproductValidate

derive newtype instance coarbitraryCoproductForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (CoproductForm f g e a)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

getLeftForm
  :: forall f c g e' a' e a
   . IsForm f c
  => CoproductForm (f e' a') g e a
  -> f e' a'
getLeftForm cf = updateContext currentLeft initialLeft
  where
  currentLeft = currentContext $ either snd fst $ unCC $ currentContext cf
  initialLeft = either snd fst $ unCC $ initialContext cf

setLeftForm
  :: forall f c g e' a' e a
   . IsForm f c
  => f e' a'
  -> CoproductForm (f e' a') g e a
  -> CoproductForm (f e' a') g e a
setLeftForm f =
  updatesContext (go currentContext) <<< loadsContext (go initialContext)
  where
  go getter = overCC $ second loader +++ first loader
    where
    loader = loadContext $ getter f

getRightForm
  :: forall f c g e' a' e a
   . IsForm g c
  => CoproductForm f (g e' a') e a
  -> g e' a'
getRightForm cf = updateContext currentRight initialRight
  where
  currentRight = currentContext $ either fst snd $ unCC $ currentContext cf
  initialRight = either fst snd $ unCC $ currentContext cf

setRightForm
  :: forall f c g e' a' e a
   . IsForm g c
  => g e' a'
  -> CoproductForm f (g e' a') e a
  -> CoproductForm f (g e' a') e a
setRightForm g =
  updatesContext (go currentContext) <<< loadsContext (go initialContext)
  where
  go getter = overCC $ first loader +++ second loader
    where
    loader = loadContext $ getter g

choose
  :: forall f g e a. Boolean -> CoproductForm f g e a -> CoproductForm f g e a
choose false = updatesContext $ overCC $ either Left (Left <<< swap)
choose true = updatesContext $ overCC $ either (Right <<< swap) Right

toggle :: forall f g e a. CoproductForm f g e a -> CoproductForm f g e a
toggle = updatesContext $ overCC $ either (Right <<< swap) (Left <<< swap)
