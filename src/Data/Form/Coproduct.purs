module Data.Form.Coproduct
  ( CoproductContext
  , CoproductForm(..)
  , coproduct
  , coproductValidate
  , getCurrentForm
  , getLeftForm
  , getRightForm
  , overCurrentForm
  , setCurrentForm
  , setLeftForm
  , setRightForm
  , toggleCurrentForm
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Either.Nested (type (\/))
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
import Data.Newtype (unwrap)
import Data.Profunctor.Choice (left, right, (+++), (|||))
import Data.Profunctor.Star (Star(..))

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data CoproductContext f g = CC Boolean Boolean f g

type CoproductForm f g = Form (CoproductContext f g)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

coproductValidate
  :: forall c1 c2 e1 e2 e a b c
   . Form c1 e1 a
  -> Form c2 e2 b
  -> (a \/ b -> Either e c)
  -> CoproductForm (Form c1 e1 a) (Form c2 e2 b) e c
coproductValidate f g validate =
  formValidate
    (CC false false f g)
    (fromEither <<< validate <=< validateCoproduct)
  where
  validateCoproduct =
    unwrap (Star ignoreError +++ Star ignoreError) <<< getCurrentForm

coproduct
  :: forall c1 c2 e1 e2 e a b
   . Form c1 e1 a
  -> Form c2 e2 b
  -> CoproductForm (Form c1 e1 a) (Form c2 e2 b) e (a \/ b)
coproduct f g = coproductValidate f g Right

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance eqCoproductContext ::
  ( Eq f
  , Eq g
  ) =>
  Eq (CoproductContext f g)

instance showCoproductContext ::
  ( Show f
  , Show g
  ) =>
  Show (CoproductContext f g) where
  show (CC i c f g) =
    "(CC " <> show i <> " " <> show c <> " " <> show f <> " " <> show g <> ")"

instance formContextCoproductContext ::
  ( FormContext c1 i
  , FormContext c2 j
  ) =>
  FormContext (CoproductContext (Form c1 e1 a) (Form c2 e2 b)) (i \/ j) where
  clear (CC _ _ f g) = CC false false (clear f) (clear g)
  current = (current +++ current) <<< getCurrentForm
  dirty = (dirty ||| dirty) <<< getCurrentForm
  load = overCurrentForm <<< either left right <<< (load +++ load)
  update = overCurrentForm <<< either left right <<< (update +++ update)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

getLeftForm :: forall f g. CoproductContext f g -> f
getLeftForm (CC _ _ f _) = f

setLeftForm
  :: forall f g. f -> CoproductContext f g -> CoproductContext f g
setLeftForm f (CC i c _ g) = CC i c f g

getRightForm :: forall f g. CoproductContext f g -> g
getRightForm (CC _ _ _ g) = g

setRightForm :: forall f g. g -> CoproductContext f g -> CoproductContext f g
setRightForm g (CC i c f _) = CC i c f g

getCurrentForm :: forall f g. CoproductContext f g -> f \/ g
getCurrentForm (CC _ false f _) = Left f
getCurrentForm (CC _ true _ g) = Right g

setCurrentForm
  :: forall f g. f \/ g -> CoproductContext f g -> CoproductContext f g
setCurrentForm (Left f) (CC i _ _ g) = CC i false f g
setCurrentForm (Right g) (CC i _ f _) = CC i true f g

overCurrentForm
  :: forall f g
   . (f \/ g -> f \/ g)
  -> CoproductContext f g
  -> CoproductContext f g
overCurrentForm f cc = setCurrentForm (f $ getCurrentForm cc) cc

toggleCurrentForm :: forall f g. CoproductContext f g -> CoproductContext f g
toggleCurrentForm (CC i choice f g) = CC i (not choice) f g
