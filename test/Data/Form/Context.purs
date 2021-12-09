module Test.Data.Form.Context where

import Prelude

import Control.Monad.Gen (frequency, resize, sized)
import Data.Form.Context (class FormContext, blank, current, initial, load, output, update)
import Data.Int (toNumber)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, arbitrary, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy)

newtype ArbitraryFormContext ctx = AFC ctx

derive instance newtypeAFC :: Newtype (ArbitraryFormContext ctx) _
derive instance eqAFC :: Eq ctx => Eq (ArbitraryFormContext ctx)
derive instance ordAFC :: Ord ctx => Ord (ArbitraryFormContext ctx)
derive newtype instance showAFC :: Show ctx => Show (ArbitraryFormContext ctx)

instance fromContextAFC ::
  FormContext ctx i o =>
  FormContext (ArbitraryFormContext ctx) i o where
  blank = AFC blank
  current (AFC ctx) = current ctx
  initial (AFC ctx) = initial ctx
  load i (AFC ctx) = AFC (load i ctx)
  output (AFC ctx) = output ctx
  update i (AFC ctx) = AFC (update i ctx)

instance arbitraryAFC ::
  ( Arbitrary i
  , FormContext ctx i o
  ) =>
  Arbitrary (ArbitraryFormContext ctx) where
  arbitrary = sized \size ->
    frequency
      $ Tuple 0.1 (pure $ AFC blank)
          :|
            [ Tuple (toNumber (size / 20)) $ load <$> arbitrary <*> resize
                (_ / 2)
                arbitrary
            , Tuple (toNumber (size / 20)) $ update <$> arbitrary <*> resize
                (_ / 2)
                arbitrary
            ]

checkFormContext
  :: forall ctx i o
   . Show i
  => Arbitrary i
  => Eq i
  => FormContext ctx i o
  => Proxy ctx
  -> Spec Unit
checkFormContext _ = do
  describe "FormContext laws" do
    it "satisfies initial <<< load" do
      quickCheck \i (AFC (ctx :: ctx)) -> initial (load i ctx) === i
    it "satisfies current <<< load" do
      quickCheck \i (AFC (ctx :: ctx)) -> current (load i ctx) === i
    it "satisfies initial <<< update" do
      quickCheck \i (AFC (ctx :: ctx)) -> initial (update i ctx) === initial ctx
    it "satisfies current <<< update" do
      quickCheck \i (AFC (ctx :: ctx)) -> current (update i ctx) === i
    it "satisfies initial blank = current blank" do
      current (blank :: ctx) `shouldEqual` initial (blank :: ctx)
