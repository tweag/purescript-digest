module Data.Form.Text
  ( TextForm(..)
  , number
  , numberRequired
  , parsed
  , text
  , textRequired
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable)
import Data.Form (Form, form', mapEither, required)
import Data.Form.Context.Text (TextContext)
import Data.Form.Required (Required)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over)
import Data.Number as Number
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Traversable (traverse)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype TextForm e a = TextForm (Form TextContext e a)

type ParsedForm e a = TextForm e (Maybe a)

type ParsedFormRequired e a = TextForm (Required e) a

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

text :: forall e. TextForm e String
text = TextForm $ form'

parsed :: forall e a. (NonEmptyString -> Either e a) -> ParsedForm e a
parsed parse =
  over TextForm (mapEither $ traverse parse <<< NES.fromString) $ text

textRequired :: forall e. ParsedFormRequired e NonEmptyString
textRequired = over TextForm required $ parsed Right

number :: ParsedForm Unit Number
number = parsed $ note unit <<< Number.fromString <<< NES.toString

numberRequired :: ParsedFormRequired Unit Number
numberRequired = over TextForm required number

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericTextContext :: Generic (TextForm e a) _

derive instance newtypeTextContext :: Newtype (TextForm e a) _

derive instance eqTextContext :: (Eq e, Eq a) => Eq (TextForm e a)

derive instance functorTextContext :: Functor (TextForm e)

derive newtype instance invariantTextContext :: Invariant (TextForm e)

derive newtype instance bifunctorTextContext :: Bifunctor TextForm

derive newtype instance foldableTextContext :: Foldable (TextForm e)

derive newtype instance bifoldableTextContext :: Bifoldable TextForm

