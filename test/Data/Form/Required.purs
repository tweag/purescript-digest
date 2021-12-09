module Test.Data.Form.Required where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Extend (class Extend)
import Control.Monad.Gen (frequency)
import Data.Foldable (class Foldable)
import Data.Form.Required (Required(..))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (perturbGen)
import Test.QuickCheck.Laws (A)
import Test.QuickCheck.Laws.Control (checkAlt, checkAlternative, checkApplicative, checkApply, checkBind, checkExtend, checkMonad, checkPlus)
import Test.QuickCheck.Laws.Data (checkBounded, checkEq, checkFoldable, checkFunctor, checkMonoid, checkOrd, checkSemigroup)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..), Proxy2(..))

newtype ArbitraryRequired e = AR (Required e)

derive newtype instance eqAR :: Eq e => Eq (ArbitraryRequired e)
derive newtype instance ordAR :: Ord e => Ord (ArbitraryRequired e)
derive newtype instance boundedAR :: Bounded e => Bounded (ArbitraryRequired e)
derive newtype instance semigroupAR ::
  Semigroup e =>
  Semigroup (ArbitraryRequired e)

derive newtype instance monoidAR :: Monoid e => Monoid (ArbitraryRequired e)
derive newtype instance functorAR :: Functor ArbitraryRequired
derive newtype instance foldableAR :: Foldable ArbitraryRequired
derive newtype instance applyAR :: Apply ArbitraryRequired
derive newtype instance applicativeAR :: Applicative ArbitraryRequired
derive newtype instance altAR :: Alt ArbitraryRequired
derive newtype instance plusAR :: Plus ArbitraryRequired
derive newtype instance alternativeAR :: Alternative ArbitraryRequired
derive newtype instance bindAR :: Bind ArbitraryRequired
derive newtype instance monadAR :: Monad ArbitraryRequired
derive newtype instance extendAR :: Extend ArbitraryRequired

instance coarbitraryAR :: Coarbitrary e => Coarbitrary (ArbitraryRequired e) where
  coarbitrary (AR Missing) = perturbGen 1.0
  coarbitrary (AR (Invalid a)) = perturbGen 2.0 <<< coarbitrary a

instance arbitraryAR :: Arbitrary e => Arbitrary (ArbitraryRequired e) where
  arbitrary =
    frequency
      $ Tuple 0.1 (pure (AR Missing))
          :| [ Tuple 0.9 (AR <<< Invalid <$> arbitrary) ]

spec :: Spec Unit
spec = do
  describe "Required" do
    it "obeys the Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (ArbitraryRequired A))
    it "obeys the Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (ArbitraryRequired A))
    it "obeys the Functor laws" do
      liftEffect $ checkFunctor (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Foldable laws" do
      liftEffect $ checkFoldable (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Apply laws" do
      liftEffect $ checkApply (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Applicative laws" do
      liftEffect $ checkApplicative (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Alt laws" do
      liftEffect $ checkAlt (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Plus laws" do
      liftEffect $ checkPlus (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Alternate laws" do
      liftEffect $ checkAlternative (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Bind laws" do
      liftEffect $ checkBind (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Monad laws" do
      liftEffect $ checkMonad (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Extend laws" do
      liftEffect $ checkExtend (Proxy2 :: _ ArbitraryRequired)
    it "obeys the Semigroup laws" do
      liftEffect $ checkSemigroup (Proxy :: _ (ArbitraryRequired A))
    it "obeys the Monoid laws" do
      liftEffect $ checkMonoid (Proxy :: _ (ArbitraryRequired A))
    it "obeys the Bounded laws" do
      liftEffect $ checkBounded (Proxy :: _ (ArbitraryRequired A))
