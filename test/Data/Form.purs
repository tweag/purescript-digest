module Test.Data.Form where

import Prelude

import Data.Foldable (class Foldable)
import Data.Form (class Form, current, initial, load, save, update)
import Data.Form.Coproduct (CoproductForm)
import Data.Form.Product (ProductForm)
import Data.Form.Traversable (TraversableForm)
import Data.List (List)
import Effect.Class (liftEffect)
import Test.QuickCheck
  ( class Arbitrary
  , class Coarbitrary
  , class Testable
  , (===)
  )
import Test.QuickCheck.Laws (A)
import Test.QuickCheck.Laws.Data (checkEq, checkFoldable, checkFunctor)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..), Proxy2(..), Proxy3(..))

checkForm
  :: forall f i
   . Eq i
  => Eq (f Int String)
  => Eq (f Int A)
  => Show (f Int String)
  => Functor (f Int)
  => Foldable (f Int)
  => Show i
  => Form f i
  => Arbitrary (f Int String)
  => Arbitrary (f Int A)
  => Coarbitrary (f Int String)
  => Arbitrary i
  => Proxy3 f
  -> Spec Unit
checkForm p = do
  checkForm' "Form" p
  checkForm' "ProductForm"
    (Proxy3 :: _ (ProductForm (f Int String) (f Int String)))
  checkForm' "CoproductForm"
    (Proxy3 :: _ (CoproductForm (f Int String) (f Int String)))
  checkForm' "TraversableForm Array"
    (Proxy3 :: _ (TraversableForm Array (f Int String)))
  checkForm' "TraversableForm List"
    (Proxy3 :: _ (TraversableForm List (f Int String)))

checkForm'
  :: forall f i
   . Eq i
  => Eq (f Int String)
  => Eq (f Int A)
  => Show (f Int String)
  => Functor (f Int)
  => Foldable (f Int)
  => Show i
  => Form f i
  => Arbitrary (f Int String)
  => Arbitrary (f Int A)
  => Coarbitrary (f Int String)
  => Arbitrary i
  => String
  -> Proxy3 f
  -> Spec Unit
checkForm' title _ = do
  describe title do
    describe "laws" do
      let
        checkLaw :: forall p. Testable p => (f Int String -> p) -> _
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
      liftEffect $ checkEq (Proxy :: _ (f Int String))
    it "obeys the Functor laws" do
      liftEffect $ checkFunctor (Proxy2 :: _ (f Int))
    it "obeys the Foldable laws" do
      liftEffect $ checkFoldable (Proxy2 :: _ (f Int))
