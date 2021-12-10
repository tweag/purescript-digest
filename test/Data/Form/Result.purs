module Test.Data.Form.Result where

import Prelude

import Data.Form.Result (Result)
import Effect.Class (liftEffect)
import Test.QuickCheck.Laws (A, B)
import Test.QuickCheck.Laws.Control
  ( checkAlt
  , checkApplicative
  , checkApply
  , checkBind
  , checkExtend
  , checkMonad
  )
import Test.QuickCheck.Laws.Data
  ( checkBounded
  , checkEq
  , checkFoldable
  , checkFunctor
  , checkOrd
  , checkSemigroup
  )
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..), Proxy2(..))

spec :: Spec Unit
spec = do
  describe "Required" do
    it "obeys the Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (Result A B))
    it "obeys the Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (Result A B))
    it "obeys the Functor laws" do
      liftEffect $ checkFunctor (Proxy2 :: _ (Result A))
    it "obeys the Foldable laws" do
      liftEffect $ checkFoldable (Proxy2 :: _ (Result A))
    it "obeys the Apply laws" do
      liftEffect $ checkApply (Proxy2 :: _ (Result A))
    it "obeys the Applicative laws" do
      liftEffect $ checkApplicative (Proxy2 :: _ (Result A))
    it "obeys the Alt laws" do
      liftEffect $ checkAlt (Proxy2 :: _ (Result A))
    it "obeys the Bind laws" do
      liftEffect $ checkBind (Proxy2 :: _ (Result A))
    it "obeys the Monad laws" do
      liftEffect $ checkMonad (Proxy2 :: _ (Result A))
    it "obeys the Extend laws" do
      liftEffect $ checkExtend (Proxy2 :: _ (Result A))
    it "obeys the Semigroup laws" do
      liftEffect $ checkSemigroup (Proxy :: _ (Result A B))
    it "obeys the Bounded laws" do
      liftEffect $ checkBounded (Proxy :: _ (Result A B))
