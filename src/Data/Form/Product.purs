module Data.Form.Product
  ( ProductForm(..)
  , ProductContext(..)
  , getFstForm
  , getSndForm
  , product
  , productValidate
  , setFstForm
  , setSndForm
  , getForms
  , overForms
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Form
  ( class FormContext
  , Form
  , clear
  , current
  , dirty
  , formValidate
  , ignoreError
  , load
  , update
  )
import Data.Form.Result (fromEither)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (second, (***))
import Data.Show.Generic (genericShow)
import Data.Tuple (curry, fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data ProductContext f g = PC f g

type ProductForm f g = Form (ProductContext f g)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

productValidate
  :: forall c1 c2 e1 e2 e a b c
   . Form c1 e1 a
  -> Form c2 e2 b
  -> (a /\ b -> Either e c)
  -> ProductForm (Form c1 e1 a) (Form c2 e2 b) e c
productValidate f g validate =
  formValidate (PC f g) (fromEither <<< validate <=< validateProduct)
  where
  validateProduct = unwrap (Star ignoreError *** Star ignoreError) <<< getForms

product
  :: forall c1 c2 e1 e2 e a b
   . Form c1 e1 a
  -> Form c2 e2 b
  -> ProductForm (Form c1 e1 a) (Form c2 e2 b) e (a /\ b)
product f g = productValidate f g Right

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericProductContext :: Generic (ProductContext f g) _

derive instance eqProductContext ::
  ( Eq f
  , Eq g
  ) =>
  Eq (ProductContext f g)

instance showProductContext ::
  ( Show f
  , Show g
  ) =>
  Show (ProductContext f g) where
  show = genericShow

instance formContextProductContext ::
  ( FormContext c1 i
  , FormContext c2 j
  ) =>
  FormContext (ProductContext (Form c1 e1 a) (Form c2 e2 b)) (i /\ j) where
  clear = overForms $ clear *** clear
  current = (current *** current) <<< getForms
  dirty = uncurry (||) <<< (dirty *** dirty) <<< getForms
  load (i /\ j) = overForms $ load i *** load j
  update (i /\ j) = overForms $ update i *** update j

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

getForms :: forall f g. ProductContext f g -> f /\ g
getForms (PC f g) = f /\ g

overForms
  :: forall f g. (f /\ g -> f /\ g) -> ProductContext f g -> ProductContext f g
overForms f = uncurry PC <<< f <<< getForms

getFstForm :: forall f g. ProductContext f g -> f
getFstForm = fst <<< getForms

setFstForm :: forall f g. f -> ProductContext f g -> ProductContext f g
setFstForm = curry $ uncurry PC <<< second getSndForm

getSndForm :: forall f g. ProductContext f g -> g
getSndForm = snd <<< getForms

setSndForm :: forall f g. g -> ProductContext f g -> ProductContext f g
setSndForm = curry $ uncurry (flip PC) <<< second getFstForm

