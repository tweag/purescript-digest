module Test.Data.Form.Text where

import Prelude

import Data.Form.Context.Text (TextContext)
import Test.Data.Form (checkForm)
import Test.Spec (Spec, describe)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = do
  describe "TextForm" do
    checkForm (Proxy :: _ TextContext)
