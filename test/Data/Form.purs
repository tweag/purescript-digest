module Test.Data.Form where

import Prelude

import Data.Foldable (class Foldable)
import Data.Form (Form, mkForm, runForm)
import Data.Form.Context (class FormContext)
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple(..), fst)
import Effect.Class (liftEffect)
import Test.Data.Form.Context (ArbitraryFormContext(..))
import Test.Data.Form.Result (ArbitraryResult(..))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (runGen, stateful)
import Test.QuickCheck.Laws (A, B)
import Test.QuickCheck.Laws.Data (checkEq, checkFoldable, checkFunctor)
import Test.Spec (Spec, it)
import Type.Proxy (Proxy(..), Proxy2(..))

newtype ArbitraryForm ctx e a = AF (Form ctx e a)

derive instance newtypeAF :: Newtype (ArbitraryForm ctx e a) _
derive newtype instance eqAF ::
  ( Eq ctx
  , Eq e
  , Eq a
  ) =>
  Eq (ArbitraryForm ctx e a)

derive newtype instance functorAF :: Functor (ArbitraryForm ctx e)
derive newtype instance foldableAF :: Foldable (ArbitraryForm ctx e)

instance arbitraryAF ::
  ( Arbitrary i
  , Arbitrary e
  , Arbitrary a
  , Coarbitrary o
  , FormContext ctx i o
  ) =>
  Arbitrary (ArbitraryForm ctx e a) where
  arbitrary =
    map AF $ mkForm <$> (un AFC <$> arbitrary) <*> validator
    where
    validator = stateful \state -> pure \o ->
      fst $ runGen (coarbitrary o (un AR <$> arbitrary)) state

instance showAF :: (Show ctx, Show e, Show a) => Show (ArbitraryForm ctx e a) where
  show (AF form) =
    let
      Tuple f ctx = runForm form
    in
      "(Form " <> show ctx <> " " <> show (f ctx) <> ")"

checkForm
  :: forall ctx i o
   . Show ctx
  => Eq i
  => Eq ctx
  => Arbitrary i
  => Coarbitrary o
  => FormContext ctx i o
  => Proxy ctx
  -> Spec Unit
checkForm _ = do
  it "obeys the Eq laws" do
    liftEffect $ checkEq (Proxy :: _ (ArbitraryForm ctx A B))
  it "obeys the Functor laws" do
    liftEffect $ checkFunctor (Proxy2 :: _ (ArbitraryForm ctx A))
  it "obeys the Foldable laws" do
    liftEffect $ checkFoldable (Proxy2 :: _ (ArbitraryForm ctx A))
