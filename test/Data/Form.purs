module Test.Data.Form where

import Prelude

import Control.Alternative (class Plus)
import Data.Eq (class Eq1)
import Data.Form
  ( class ArbitraryFormContext
  , class FormContext
  , Form
  , current
  , initial
  , load
  , save
  , update
  )
import Data.Form.Coproduct (CoproductContext)
import Data.Form.Product (ProductContext)
import Data.Form.Traversable (TraversableContext)
import Data.Traversable (class Traversable)
import Effect.Class (liftEffect)
import Test.QuickCheck
  ( class Arbitrary
  , class Coarbitrary
  , class Testable
  , (===)
  )
import Test.QuickCheck.Laws (A, B)
import Test.QuickCheck.Laws.Data (checkEq, checkFoldable, checkFunctor)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..), Proxy2(..))

checkForm
  :: forall ctx i o
   . Show ctx
  => Show i
  => Eq i
  => Eq ctx
  => Coarbitrary o
  => Arbitrary i
  => Arbitrary ctx
  => FormContext ctx i o
  => Proxy ctx
  -> Spec Unit
checkForm _ = do
  describe "Form laws" do
    let
      checkLaw :: forall p. Testable p => (Form ctx A B -> p) -> _
      checkLaw = quickCheck
    it "satisfies initial <<< load" do
      checkLaw \form i -> initial (load i form) === i
    it "satisfies current <<< load" do
      checkLaw \form i -> current (load i form) === i
    it "satisfies initial <<< update" do
      checkLaw \form i -> initial (update i form) === initial form
    it "satisfies current <<< update" do
      checkLaw \form i -> current (update i form) === i
    it "satisfies save law" do
      checkLaw \form -> initial (save form) === current form
  it "obeys the Eq laws" do
    liftEffect $ checkEq (Proxy :: _ (Form ctx A B))
  it "obeys the Functor laws" do
    liftEffect $ checkFunctor (Proxy2 :: _ (Form ctx A))
  it "obeys the Foldable laws" do
    liftEffect $ checkFoldable (Proxy2 :: _ (Form ctx A))

checkCoproductForm
  :: forall c1 c2 i1 i2 o1 o2 e1 e2 a b
   . Show c1
  => Show c2
  => Show i1
  => Show i2
  => Show e1
  => Show e2
  => Show a
  => Show b
  => Eq c1
  => Eq c2
  => Eq i1
  => Eq i2
  => Eq e1
  => Eq e2
  => Eq a
  => Eq b
  => Arbitrary c1
  => Arbitrary c2
  => Arbitrary i1
  => Arbitrary i2
  => Arbitrary e1
  => Arbitrary e2
  => Arbitrary a
  => Arbitrary b
  => Coarbitrary o1
  => Coarbitrary o2
  => Coarbitrary a
  => Coarbitrary b
  => Coarbitrary e1
  => Coarbitrary e2
  => ArbitraryFormContext c1 i1 o1
  => ArbitraryFormContext c2 i2 o2
  => Proxy c1
  -> Proxy c2
  -> Proxy e1
  -> Proxy e2
  -> Proxy a
  -> Proxy b
  -> Spec Unit
checkCoproductForm _ _ _ _ _ _ = do
  describe "CoproductForm" do
    checkForm (Proxy :: _ (CoproductContext (Form c1 e1 a) (Form c2 e2 b)))
    checkTraversableForm
      (Proxy2 :: _ Array)
      (Proxy :: _ (CoproductContext (Form c1 e1 a) (Form c2 e2 b)))

checkProductForm
  :: forall c1 c2 i1 i2 o1 o2 e1 e2 a b
   . Show c1
  => Show c2
  => Show i1
  => Show i2
  => Show e1
  => Show e2
  => Show a
  => Show b
  => Eq c1
  => Eq c2
  => Eq i1
  => Eq i2
  => Eq e1
  => Eq e2
  => Eq a
  => Eq b
  => Arbitrary c1
  => Arbitrary c2
  => Arbitrary i1
  => Arbitrary i2
  => Arbitrary e1
  => Arbitrary e2
  => Arbitrary a
  => Arbitrary b
  => Coarbitrary o1
  => Coarbitrary o2
  => Coarbitrary a
  => Coarbitrary b
  => Coarbitrary e1
  => Coarbitrary e2
  => ArbitraryFormContext c1 i1 o1
  => ArbitraryFormContext c2 i2 o2
  => Proxy c1
  -> Proxy c2
  -> Proxy e1
  -> Proxy e2
  -> Proxy a
  -> Proxy b
  -> Spec Unit
checkProductForm _ _ e1 e2 a b = do
  describe "ProductForm" do
    checkForm (Proxy :: _ (ProductContext (Form c1 e1 a) (Form c2 e2 b)))
    checkCoproductForm
      (Proxy :: _ (CoproductContext (Form c1 e1 a) (Form c2 e2 b)))
      (Proxy :: _ (CoproductContext (Form c2 e2 b) (Form c1 e1 a)))
      e1
      e2
      a
      b

checkTraversableForm
  :: forall t ctx i o
   . Show ctx
  => Show (t ctx)
  => Show (t i)
  => Eq1 t
  => Eq (t i)
  => Eq i
  => Eq ctx
  => Coarbitrary (t ctx)
  => Arbitrary (t i)
  => Arbitrary (t ctx)
  => Arbitrary ctx
  => ArbitraryFormContext ctx i o
  => Plus t
  => Traversable t
  => Proxy2 t
  -> Proxy ctx
  -> Spec Unit
checkTraversableForm _ _ = do
  describe "TraversableForm" do
    checkForm (Proxy :: _ (TraversableContext t ctx))
