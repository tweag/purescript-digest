module Data.Form.Lenses where

import Prelude

import Data.Either (Either)
import Data.Form
  ( class FormContext
  , Form
  , current
  , getContext
  , result
  , setContext
  , update
  )
import Data.Form.Coproduct
  ( CoproductForm
  , getCurrentForm
  , getLeftForm
  , getRightForm
  , setCurrentForm
  , setLeftForm
  , setRightForm
  )
import Data.Form.Product
  ( ProductForm
  , getFstForm
  , getSndForm
  , setFstForm
  , setSndForm
  )
import Data.Form.Record (RecordForm, getPropForm, setPropForm)
import Data.Form.Result (_Error, _Ok)
import Data.Lens (Fold', Lens', lens, to)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Type.Proxy (Proxy)

context :: forall ctx e a. Lens' (Form ctx e a) ctx
context = lens getContext $ flip setContext

input :: forall ctx i e a. FormContext ctx i => Lens' (Form ctx e a) i
input = lens current $ flip update

fstForm :: forall f g e a. Lens' (ProductForm f g e a) f
fstForm = context <<< lens getFstForm (flip setFstForm)

sndForm :: forall f g e a. Lens' (ProductForm f g e a) g
sndForm = context <<< lens getSndForm (flip setSndForm)

leftForm :: forall f g e a. Lens' (CoproductForm f g e a) f
leftForm = context <<< lens getLeftForm (flip setLeftForm)

rightForm :: forall f g e a. Lens' (CoproductForm f g e a) g
rightForm = context <<< lens getRightForm (flip setRightForm)

currentForm :: forall f g e a. Lens' (CoproductForm f g e a) (Either f g)
currentForm = context <<< lens getCurrentForm (flip setCurrentForm)

propForm
  :: forall label f rf rf' e a
   . IsSymbol label
  => Row.Cons label f rf' rf
  => Row.Lacks label rf'
  => Proxy label
  -> Lens' (RecordForm rf e a) f
propForm prop = lens (getPropForm prop) $ flip $ setPropForm prop

output :: forall r ctx e a. Monoid r => Fold' r (Form ctx e a) a
output = to result <<< _Ok

error :: forall r ctx e a. Monoid r => Fold' r (Form ctx e a) e
error = to result <<< _Error
