module Test.Data.Form where

import Prelude

import Data.Form (class FormContext, Form, current, initial, load, update)
import Data.Form.Product (ProductContext)
import Effect.Class (liftEffect)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, (===))
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
  describe "FormContext laws" do
    it "satisfies initial <<< load" do
      quickCheck \(ctx :: ctx) i -> initial (load i ctx) === i
    it "satisfies current <<< load" do
      quickCheck \(ctx :: ctx) i -> current (load i ctx) === i
    it "satisfies initial <<< update" do
      quickCheck \(ctx :: ctx) i -> initial (update i ctx) === initial ctx
    it "satisfies current <<< update" do
      quickCheck \(ctx :: ctx) i -> current (update i ctx) === i
  it "obeys the Eq laws" do
    liftEffect $ checkEq (Proxy :: _ (Form ctx A B))
  it "obeys the Functor laws" do
    liftEffect $ checkFunctor (Proxy2 :: _ (Form ctx A))
  it "obeys the Foldable laws" do
    liftEffect $ checkFoldable (Proxy2 :: _ (Form ctx A))

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
  => FormContext c1 i1 o1
  => FormContext c2 i2 o2
  => Proxy c1
  -> Proxy c2
  -> Proxy e1
  -> Proxy e2
  -> Proxy a
  -> Proxy b
  -> Spec Unit
checkProductForm _ _ _ _ _ _ = do
  describe "ProductForm" do
    checkForm (Proxy :: _ (ProductContext (Form c1 e1 a) (Form c2 e2 b)))
