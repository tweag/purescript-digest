module Data.Form.Traversable
  ( TraversableContext(..)
  , TraversableForm
  , TraversableFormF
  , TraversableFormV
  , traversable
  , traversableF
  , traversableV
  ) where

import Prelude

import Control.Alternative (class Plus, empty)
import Control.Comonad (extract)
import Data.Align (class Align, align)
import Data.Bifunctor (lmap)
import Data.Compactable (class Compactable, compact)
import Data.Form
  ( class FormContext
  , Form
  , FormF
  , FormV
  , current
  , extractF
  , form
  , formF
  , update
  )
import Data.Maybe (Maybe(..))
import Data.These (these)
import Data.Traversable (class Traversable, traverse)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data TraversableContext t s = TraversableContext s (t s)

instance formContextTraversableContext ::
  ( FormContext s i
  , Align t
  , Compactable t
  ) =>
  FormContext (TraversableContext t (Form s a)) (t i) where
  current (TraversableContext _ ts) = current <$> ts
  update ti (TraversableContext f ts) =
    TraversableContext f
      $ compact
      $ align
          ( these
              (Just <<< flip update f)
              (const Nothing)
              (compose Just <<< update)
          )
          ti
          ts

type TraversableForm t s = Form (TraversableContext t s)
type TraversableFormF f t s a = FormF f (TraversableContext t s) a
type TraversableFormV e t s a = FormV e (TraversableContext t s) a

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

traversable
  :: forall t s a
   . Plus t
  => Functor t
  => Form s a
  -> TraversableForm t (Form s a) (t a)
traversable f =
  form (\(TraversableContext _ ts) -> map extract ts)
    $ TraversableContext f empty

traversableF
  :: forall f t s a
   . Plus t
  => Traversable t
  => Applicative f
  => FormF f s a
  -> TraversableFormF f t (FormF f s a) (t a)
traversableF f =
  formF (\(TraversableContext _ ts) -> traverse extractF ts)
    $ TraversableContext f empty

traversableV
  :: forall e t s a
   . Plus t
  => Traversable t
  => FormV e s a
  -> TraversableFormV Unit t (FormV e s a) (t a)
traversableV f =
  formF
    ( \(TraversableContext _ ts) ->
        traverse (lmap (const unit) <<< extractF) ts
    ) $ TraversableContext f empty
