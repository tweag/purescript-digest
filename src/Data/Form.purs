module Data.Form
  ( Form
  , collectResults
  , extendResult
  , extractResult
  , form
  , form'
  , imapContext
  , mapEither
  , overContext
  , peekResult
  , peeksResult
  , required
  , setContext
  , viewContext
  ) where

import Prelude

import Control.Comonad (extend, extract)
import Control.Comonad.Store (Store, experiment, peek, peeks, pos, runStore, seek, seeks, store)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either, either, hush, note)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Form.Context (class FormContext, blank, output)
import Data.Form.Required (Required(..))
import Data.Form.Result (Result(..), toEither)
import Data.Function (on)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Maybe (Maybe)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..), uncurry)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype Form ctx e a = Form (Store ctx (Result e a))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance functorForm :: Functor (Form ctx e)

instance invariantForm :: Invariant (Form ctx e) where
  imap = imapF

instance bifunctorForm :: Bifunctor (Form ctx) where
  bimap f g = overForm $ map $ bimap f g

instance foldableForm :: Foldable (Form ctx e) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = foldMap f <<< extractResult

instance bifoldableForm :: Bifoldable (Form ctx) where
  bifoldr f = bifoldrDefault f
  bifoldl f = bifoldlDefault f
  bifoldMap f g = bifoldMap f g <<< extractResult

instance eqForm :: (Eq ctx, Eq e, Eq a) => Eq (Form ctx e a) where
  eq = on eq $ uncurry evalStore <<< runStore <<< unForm
    where
    evalStore p s = Tuple (p s) s

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

form
  :: forall ctx i o e a
   . FormContext ctx i o
  => (o -> Result e a)
  -> Form ctx e a
form validate = Form $ store (validate <<< output) blank

form' :: forall ctx i o e. FormContext ctx i o => Form ctx e o
form' = form Ok

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

imapContext
  :: forall ctx ctx' e a
   . (ctx' -> ctx)
  -> (ctx -> ctx')
  -> Form ctx e a
  -> Form ctx' e a
imapContext f g = overForm (uncurry store <<< uncurry go <<< runStore)
  where
  go p s = Tuple (lcmap f p) $ g s

extendResult
  :: forall ctx e f a b
   . (Form ctx e a -> Result f b)
  -> Form ctx e a
  -> Form ctx f b
extendResult f = overForm $ extend $ f <<< Form

mapEither
  :: forall ctx e a b . (a -> Either e b) -> Form ctx e a -> Form ctx e b
mapEither f = extendResult $ either Error Ok <<< f <=< extractResult

setContext :: forall ctx e a. ctx -> Form ctx e a -> Form ctx e a
setContext ctx = overForm $ seek ctx

overContext :: forall ctx e a. (ctx -> ctx) -> Form ctx e a -> Form ctx e a
overContext f = overForm $ seeks f

extractResult :: forall ctx e a. Form ctx e a -> Result e a
extractResult = extract <<< unForm

peekResult :: forall ctx e a. ctx -> Form ctx e a -> Result e a
peekResult ctx = peek ctx <<< unForm

peeksResult :: forall ctx e a. (ctx -> ctx) -> Form ctx e a -> Result e a
peeksResult f = peeks f <<< unForm

required :: forall ctx e a. Form ctx e (Maybe a) -> Form ctx (Required e) a
required = mapEither (note Missing) <<< lmap Invalid

-------------------------------------------------------------------------------
-- Eliminators
-------------------------------------------------------------------------------

viewContext :: forall ctx e a. Form ctx e a -> ctx
viewContext = pos <<< unForm

collectResults
  :: forall f ctx e a
   . Functor f
  => Filterable f
  => (ctx -> f ctx)
  -> Form ctx e a
  -> f a
collectResults f =
  filterMap (hush <<< toEither) <<< experiment f <<< unForm

-------------------------------------------------------------------------------
-- Private helpers
-------------------------------------------------------------------------------

unForm :: forall ctx e a. Form ctx e a -> Store ctx (Result e a)
unForm (Form s) = s

overForm
  :: forall ctx ctx' e f a b
   . (Store ctx (Result e a) -> Store ctx' (Result f b))
  -> Form ctx e a
  -> Form ctx' f b
overForm f = Form <<< f <<< unForm

