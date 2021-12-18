module Data.Form.Lenses where

import Prelude

import Control.Comonad.Store (class ComonadStore, pos, seek)
import Data.Form (class FormContext, Form, current, update)
import Data.Lens (Lens', lens)

_pos :: forall s w a. ComonadStore s w => Lens' (w a) s
_pos = lens pos $ flip seek

input :: forall s i a. FormContext s i => Lens' (Form s a) i
input = lens current $ flip update
