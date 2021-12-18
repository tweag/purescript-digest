module Data.Form.Zip
  ( ZipForm
  , ZipFormF
  , ZipFormV
  , zip
  , zipF
  , zipV
  , zipWith
  , zipWithF
  , zipWithV
  ) where

import Prelude

import Control.Comonad (extract)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Form (Form, FormF, FormV, extractF, form, formF, formV)
import Data.Maybe (Maybe)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..), curry, uncurry)
import Data.Tuple.Nested (type (/\))

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

type ZipForm s t = Form (s /\ t)
type ZipFormF f s t = FormF f (s /\ t)
type ZipFormV e s t a = FormV e (s /\ t) a

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

zipWith
  :: forall s t a b c
   . (a -> b -> c)
  -> Form s a
  -> Form t b
  -> ZipForm (Form s a) (Form t b) c
zipWith f = curry $ form $ uncurry f <<< (extract *** extract)

zip
  :: forall s t a b
   . Form s a
  -> Form t b
  -> ZipForm (Form s a) (Form t b) (a /\ b)
zip = zipWith Tuple

zipWithF
  :: forall f s t a b c
   . Bind f
  => (a -> b -> f c)
  -> FormF f s a
  -> FormF f t b
  -> ZipFormF f (FormF f s a) (FormF f t b) c
zipWithF k f g = formF k' $ Tuple f g
  where
  k' (Tuple f' g') = do
    a <- extractF f'
    b <- extractF g'
    k a b

zipF
  :: forall f s t a b
   . Monad f
  => FormF f s a
  -> FormF f t b
  -> ZipFormF f (FormF f s a) (FormF f t b) (a /\ b)
zipF = zipWithF \a b -> pure $ Tuple a b

zipWithV
  :: forall x y s t a b c
   . (a -> b -> Maybe c)
  -> FormV x s a
  -> FormV y t b
  -> ZipFormV Unit (FormV x s a) (FormV y t b) c
zipWithV k f g = formV k' $ Tuple f g
  where
  k' (Tuple f' g') = do
    a <- lmap (const unit) $ extractF f'
    b <- lmap (const unit) $ extractF g'
    note unit $ k a b

zipV
  :: forall x y s t a b
   . FormV x s a
  -> FormV y t b
  -> ZipFormV Unit (FormV x s a) (FormV y t b) (a /\ b)
zipV = zipWithV $ compose pure <<< Tuple
