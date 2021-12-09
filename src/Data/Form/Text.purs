module Data.Form.Text
  ( TextForm(..)
  , TextContext
  , number
  , numberRequired
  , parsed
  , text
  , textRequired
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Either (Either(..), note)
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Form (class FormContext, arbitraryFormContext, Form, mkForm, required)
import Data.Form.Required (Required)
import Data.Form.Result (Result(..), fromEither)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.Number as Number
import Data.String (joinWith, null, trim)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Traversable (traverse)
import Test.QuickCheck (class Arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data TextContext = TextContext String String

newtype TextForm e a = TextForm (Form TextContext e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

text :: forall e. TextForm e String
text = TextForm $ mkForm (TextContext "" "") Ok

parsed :: forall e a. (String -> Either e a) -> TextForm e a
parsed parse =
  TextForm
    $ mkForm (TextContext "" "")
    $ fromEither <<< lmap Just <<< parse

parsedNonEmpty :: forall e a. (String -> Either e a) -> TextForm e (Maybe a)
parsedNonEmpty parse =
  TextForm
    $ mkForm (TextContext "" "")
    $ traverse (fromEither <<< lmap Just <<< parse)
        <<< filter (not null)
        <<< Just
        <<< trim

textRequired :: forall e. TextForm (Required e) NonEmptyString
textRequired = over TextForm required $ parsed (Right <<< NES.fromString)

number :: TextForm Unit (Maybe Number)
number = parsedNonEmpty $ note unit <<< Number.fromString

numberRequired :: TextForm (Required Unit) Number
numberRequired = over TextForm required number

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericTextForm :: Generic (TextForm e a) _

derive instance newtypeTextForm :: Newtype (TextForm e a) _

derive instance eqTextForm :: (Eq e, Eq a) => Eq (TextForm e a)

derive instance functorTextForm :: Functor (TextForm e)

derive newtype instance invariantTextForm :: Invariant (TextForm e)

derive newtype instance bifunctorTextForm :: Bifunctor TextForm

derive newtype instance foldableTextForm :: Foldable (TextForm e)

derive newtype instance bifoldableTextForm :: Bifoldable TextForm

derive newtype instance arbitraryTextForm ::
  ( Arbitrary e
  , Arbitrary a
  ) =>
  Arbitrary (TextForm e a)

derive instance eqTextContext :: Eq TextContext

derive instance ordTextContext :: Ord TextContext

instance showTextContext :: Show TextContext where
  show (TextContext init curr) = joinWith ""
    [ "(TextContext "
    , show init
    , " "
    , show curr
    , ")"
    ]

instance formContextTextContext :: FormContext TextContext String String where
  current (TextContext _ curr) = curr
  initial (TextContext init _) = init
  load input _ = TextContext input input
  output (TextContext _ curr) = curr
  update input (TextContext init _) = TextContext init input

instance arbitraryTextContext :: Arbitrary TextContext where
  arbitrary = arbitraryFormContext $ pure (TextContext "" "")
