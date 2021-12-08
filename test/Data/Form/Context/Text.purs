module Test.Data.Form.Context.Text where

import Prelude

import Data.Form.Context.Text (TextContext)
import Effect.Class (liftEffect)
import Test.Data.Form.Context (ArbitraryFormContext, checkFormContext)
import Test.QuickCheck.Laws.Data (checkEq, checkOrd)
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = do
  describe "TextContext" do
    checkFormContext (Proxy :: _ TextContext)
    it "obeys the Eq laws" do
      liftEffect $ checkEq (Proxy :: _ (ArbitraryFormContext TextContext))
    it "obeys the Ord laws" do
      liftEffect $ checkOrd (Proxy :: _ (ArbitraryFormContext TextContext))
