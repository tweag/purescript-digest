module Data.Form.Text
  ( NumberFormOptional
  , NumberFormRequired
  , ParsedForm
  , ParsedFormOptional
  , StringForm
  , StringFormRequired
  , TextForm(..)
  , TextFormOptional
  , TextFormRequired
  , numberOptional
  , numberRequired
  , text
  , textOptional
  , textParsed
  , textParsedOptional
  , textRequired
  , textValidate
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Bifoldable
  ( class Bifoldable
  , bifoldMap
  , bifoldlDefault
  , bifoldrDefault
  )
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..), note)
import Data.Filterable (filter)
import Data.Foldable (class Foldable)
import Data.Form
  ( Form(..)
  , arbitraryForm
  , formValidate
  , mapResult
  , required
  , result
  )
import Data.Form.Result (fromEither)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number as Number
import Data.String (null, trim)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Traversable (traverse)
import Test.QuickCheck (class Arbitrary, class Coarbitrary)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (eof)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype TextForm e a = TextForm (Form (Identity String) e a)
type StringForm e = TextForm e String
type StringFormRequired e = TextFormRequired e NonEmptyString
type TextFormOptional e a = TextForm e (Maybe a)
type TextFormRequired e = TextForm (Maybe e)
type NumberFormOptional = TextFormOptional Unit Number
type NumberFormRequired = TextFormRequired Unit Number
type ParsedFormOptional a = TextFormOptional ParseError a
type ParsedForm = TextForm ParseError

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

textValidate :: forall e a. (String -> Either e a) -> TextForm e a
textValidate f = formValidate (Identity "") $ fromEither <<< f <<< unwrap

text :: forall e. StringForm e
text = textValidate Right

textOptional :: forall e a. (String -> Either e a) -> TextFormOptional e a
textOptional f = textValidate $ traverse f <<< filter (not null) <<< Just <<<
  trim

textParsedOptional :: forall a. Parser String a -> ParsedFormOptional a
textParsedOptional parser =
  textValidate $ flip runParser $ (Nothing <$ eof) <|> (Just <$> parser)

textParsed :: forall a. Parser String a -> ParsedForm a
textParsed = textValidate <<< flip runParser

textRequired :: forall e. StringFormRequired e
textRequired = required $ textValidate (Right <<< NES.fromString)

numberOptional :: NumberFormOptional
numberOptional = textOptional $ note unit <<< Number.fromString

numberRequired :: NumberFormRequired
numberRequired = required numberOptional

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericTextForm :: Generic (TextForm e a) _

derive instance newtypeTextForm :: Newtype (TextForm e a) _

derive instance eqTextForm :: (Eq e, Eq a) => Eq (TextForm e a)

derive newtype instance showTextForm :: (Show e, Show a) => Show (TextForm e a)

derive instance functorTextForm :: Functor (TextForm e)

derive newtype instance invariantTextForm :: Invariant (TextForm e)

instance bifunctorTextForm :: Bifunctor TextForm where
  bimap f g = mapResult $ bimap f g

derive newtype instance foldableTextForm :: Foldable (TextForm e)

instance bifoldableTextForm :: Bifoldable TextForm where
  bifoldMap f g = bifoldMap f g <<< result
  bifoldr f = bifoldrDefault f
  bifoldl f = bifoldlDefault f

instance arbitraryTextForm ::
  ( Arbitrary a
  , Arbitrary e
  ) =>
  Arbitrary (TextForm e a) where
  arbitrary = arbitraryForm $ const textValidate

derive newtype instance coarbitraryTextForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (TextForm e a)
