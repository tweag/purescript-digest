module Test.Data.Form.Result where

import Prelude

import Control.Alt (class Alt)
import Control.Extend (class Extend)
import Control.Monad.Gen (frequency)
import Data.Foldable (class Foldable)
import Data.Form.Result (Result(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (perturbGen)
import Test.QuickCheck.Laws (A, B)
import Test.QuickCheck.Laws.Control (checkAlt, checkApplicative, checkApply, checkBind, checkExtend, checkMonad)
import Test.QuickCheck.Laws.Data (checkBounded, checkEq, checkFoldable, checkFunctor, checkOrd, checkSemigroup)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..), Proxy2(..))

newtype ArbitraryResult e a = AR (Result e a)

derive instance newtypeAR :: Newtype (ArbitraryResult e a) _
derive newtype instance eqAR :: (Eq e, Eq a) => Eq (ArbitraryResult e a)
derive newtype instance ordAR :: (Ord e, Ord a) => Ord (ArbitraryResult e a)
derive newtype instance boundedAR ::
  ( Bounded e
  , Bounded a
  ) =>
  Bounded (ArbitraryResult e a)

derive newtype instance semigroupAR ::
  Semigroup a =>
  Semigroup (ArbitraryResult e a)

derive newtype instance functorAR :: Functor (ArbitraryResult e)
derive newtype instance foldableAR :: Foldable (ArbitraryResult e)
derive newtype instance applyAR :: Apply (ArbitraryResult e)
derive newtype instance applicativeAR :: Applicative (ArbitraryResult e)
derive newtype instance altAR :: Alt (ArbitraryResult e)
derive newtype instance bindAR :: Bind (ArbitraryResult e)
derive newtype instance monadAR :: Monad (ArbitraryResult e)
derive newtype instance extendAR :: Extend (ArbitraryResult e)

instance coarbitraryAR ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (ArbitraryResult e a) where
  coarbitrary (AR Unevaluated) = perturbGen 1.0
  coarbitrary (AR (Error e)) = perturbGen 2.0 <<< coarbitrary e
  coarbitrary (AR (Ok a)) = perturbGen 3.0 <<< coarbitrary a

instance arbitraryAR ::
  ( Arbitrary e
  , Arbitrary a
  ) =>
  Arbitrary (ArbitraryResult e a) where
  arbitrary =
    frequency
      $ Tuple 0.1 (pure (AR Unevaluated))
          :|
            [ Tuple 0.4 (AR <<< Error <$> arbitrary)
            , Tuple 0.5 (AR <<< Ok <$> arbitrary)
            ]

spec :: Spec Unit
spec = do
  describe "Required" do
    it "obeys the Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (ArbitraryResult A B))
    it "obeys the Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (ArbitraryResult A B))
    it "obeys the Functor laws" do
      liftEffect $ checkFunctor (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Foldable laws" do
      liftEffect $ checkFoldable (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Apply laws" do
      liftEffect $ checkApply (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Applicative laws" do
      liftEffect $ checkApplicative (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Alt laws" do
      liftEffect $ checkAlt (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Bind laws" do
      liftEffect $ checkBind (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Monad laws" do
      liftEffect $ checkMonad (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Extend laws" do
      liftEffect $ checkExtend (Proxy2 :: _ (ArbitraryResult A))
    it "obeys the Semigroup laws" do
      liftEffect $ checkSemigroup (Proxy :: _ (ArbitraryResult A B))
    it "obeys the Bounded laws" do
      liftEffect $ checkBounded (Proxy :: _ (ArbitraryResult A B))
