module Data.Form.Text where

import Prelude

import Control.Alt ((<|>))
import Control.Semigroupoid (composeFlipped)
import Data.Either (Either, note)
import Data.Filterable (filter)
import Data.Form (Form, FormF, FormV, formEmpty, formEmptyF)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.String (null, trim)
import Data.Traversable (traverse)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (eof)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data NaN = NaN

type TextForm = Form (Identity String)
type TextFormF f a = FormF f (Identity String) a
type TextFormV e a = FormV e (Identity String) a

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

string :: TextForm String
string = formEmpty unwrap

text :: forall a. (String -> a) -> TextForm a
text = formEmpty <<< composeFlipped unwrap

textF :: forall f a. (String -> f a) -> TextFormF f a
textF = formEmptyF <<< composeFlipped unwrap

textV :: forall e a. (String -> Either e a) -> TextFormV e a
textV = textF

textNonEmpty :: forall a. (String -> a) -> TextForm (Maybe a)
textNonEmpty f = text $ map f <<< filter (not null) <<< Just <<< trim

textNonEmptyF
  :: forall f a. Applicative f => (String -> f a) -> TextFormF f (Maybe a)
textNonEmptyF f = textF $ traverse f <<< filter (not null) <<< Just <<< trim

textNonEmptyV :: forall e a. (String -> Either e a) -> TextFormV e (Maybe a)
textNonEmptyV = textNonEmptyF

number :: TextFormV NaN (Maybe Number)
number = textNonEmptyV $ note NaN <<< Number.fromString

parsed :: forall a. Parser String a -> TextFormV ParseError a
parsed parser = textV $ flip runParser parser

parsedNonEmpty :: forall a. Parser String a -> TextFormV ParseError (Maybe a)
parsedNonEmpty parser = parsed $ (Nothing <$ eof) <|> (Just <$> parser)
