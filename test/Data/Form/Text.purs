module Test.Data.Form.Text where

import Prelude

import Data.Form.Text (TextContext)
import Effect.Class (liftEffect)
import Test.Data.Form (checkCoproductForm, checkForm, checkProductForm)
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = do
  describe "TextForm" do
    checkForm (Proxy :: _ TextContext)
    checkProductForm
      (Proxy :: _ TextContext)
      (Proxy :: _ TextContext)
      (Proxy :: _ Int)
      (Proxy :: _ Int)
      (Proxy :: _ Int)
      (Proxy :: _ Int)
    checkCoproductForm
      (Proxy :: _ TextContext)
      (Proxy :: _ TextContext)
      (Proxy :: _ Int)
      (Proxy :: _ Int)
      (Proxy :: _ Int)
      (Proxy :: _ Int)
  describe "TextContext" do
    it "obeys the Eq laws" do
      liftEffect $ checkEq (Proxy :: _ TextContext)
    it "obeys the Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ TextContext)
