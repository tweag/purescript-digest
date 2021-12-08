module Data.Form.Context.Text
  ( TextContext
  ) where

import Prelude

import Data.Form.Context (class FormContext)
import Data.String (joinWith)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data TextContext = TextContext String String

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

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
  blank = TextContext "" ""
  current (TextContext _ curr) = curr
  initial (TextContext init _) = init
  load input _ = TextContext input input
  output (TextContext _ curr) = curr
  update input (TextContext init _) = TextContext init input
