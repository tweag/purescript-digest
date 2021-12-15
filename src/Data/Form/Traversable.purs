module Data.Form.Traversable
  ( TraversableContext
  , TraversableForm(..)
  , traversable
  , traversableValidate
  ) where

import Prelude

import Data.Align (class Align, class Alignable, aligned, nil)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (class Foldable, foldMap)
import Data.Form
  ( class FormContext
  , Form
  , clear
  , current
  , formValidate
  , getContext
  , ignoreError
  , peekResult
  , setContext
  )
import Data.Form.Result (fromEither)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (class Newtype, alaF, over, unwrap)
import Data.NonEmpty (NonEmpty, tail, (:|))
import Data.These (These(..), these, theseLeft, theseRight)
import Data.Traversable (class Traversable, traverse)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype Aligned t a = Aligned (t (These a a))

derive instance newtypeAligned :: Newtype (Aligned t a) _

instance eqAligned :: (Eq1 t, Eq a) => Eq (Aligned t a) where
  eq = on eq1 $ unwrap

instance eq1Aligned :: (Eq1 t) => Eq1 (Aligned t) where
  eq1 = eq1

derive newtype instance showAligned ::
  Show (t (These a a)) =>
  Show (Aligned t a)

newtype TraversableContext t a = TC (NonEmpty (Aligned t) a)

unTC :: forall t a. TraversableContext t a -> t (These a a)
unTC (TC t) = unwrap $ tail t

overTC
  :: forall t a
   . (a -> t (These a a) -> t (These a a))
  -> TraversableContext t a
  -> TraversableContext t a
overTC f (TC (template :| t)) = TC $ template :| over Aligned (f template) t

type TraversableForm t f = Form (TraversableContext t f)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

traversableValidate
  :: forall t ctx i e' e a b
   . Alignable t
  => Filterable t
  => Traversable t
  => FormContext ctx i
  => Form ctx e' a
  -> (t a -> Either e b)
  -> TraversableForm t (Form i e' a) e b
traversableValidate template validate =
  formValidate
    (TC (template' :| Aligned nil))
    (fromEither <<< validate <=< validateTraversable)
  where
  template' = formValidate (current $ clear template) $ flip peekResult template
  validateTraversable =
    traverse ignoreError <<< filterMap theseRight <<< unTC

traversable
  :: forall t ctx i e' e a
   . Alignable t
  => Filterable t
  => Traversable t
  => FormContext ctx i
  => Form ctx e' a
  -> TraversableForm t (Form i e' a) e (t a)
traversable f = traversableValidate f Right

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance eqTraversableContext ::
  ( Eq1 t
  , Eq i
  ) =>
  Eq (TraversableContext t i)

derive newtype instance showTraversableContext ::
  ( Show (t (These f f))
  , Show f
  ) =>
  Show (TraversableContext t f)

instance formContextTraversableContext ::
  ( Eq i
  , Align t
  , Foldable t
  , Filterable t
  ) =>
  FormContext (TraversableContext t (Form i e' a)) (t i) where
  clear = overTC $ const $ filterMap $ map This <<< theseLeft
  current = filterMap (map getContext <<< theseRight) <<< unTC
  dirty = not <<< alaF Conj foldMap clean <<< unTC
    where
    clean = these (const false) (const false) $ on eq getContext
  load ti =
    overTC \template -> const $ map (This <<< flip setContext template) ti
  update ti = overTC \template -> filterMap (update' template) <<< aligned ti
    where
    update'
      :: Form i e' a
      -> These i (These (Form i e' a) (Form i e' a))
      -> Maybe (These (Form i e' a) (Form i e' a))
    update' template = case _ of
      This input -> Just $ That $ setContext input template
      That t -> This <$> theseLeft t
      Both input (This initial) ->
        Just $ Both initial $ setContext input initial
      Both input (That current) -> Just $ That $ setContext input current
      Both input (Both initial current) ->
        Just $ Both initial $ setContext input current
