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
import Data.Either (Either(..), note)
import Data.Filterable (filter)
import Data.Form (Form, current, formValidate, required)
import Data.Form.Result (fromEither)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String (null, trim)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (eof)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

type TextForm = Form (String /\ String)
type StringForm e = TextForm e String
type StringFormRequired e = TextFormRequired e NonEmptyString
type TextFormOptional e a = TextForm e (Maybe a)
type TextFormRequired e a = TextForm (Maybe e) a
type NumberFormOptional = TextFormOptional Unit Number
type NumberFormRequired = TextFormRequired Unit Number
type ParsedFormOptional a = TextFormOptional ParseError a
type ParsedForm a = TextForm ParseError a

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

textValidate :: forall e a. (String -> Either e a) -> TextForm e a
textValidate f = formValidate ("" /\ "") $ fromEither <<< f <<< current

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
