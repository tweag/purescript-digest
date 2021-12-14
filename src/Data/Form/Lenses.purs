module Data.Form.Lenses where

import Prelude

import Data.Form
  ( class FormContext
  , class IsForm
  , current
  , initial
  , load
  , result
  , update
  )
import Data.Form.Coproduct
  ( CoproductForm
  , getLeftForm
  , getRightForm
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

_current
  :: forall f ctx i e a. IsForm f ctx => FormContext ctx i => Lens' (f e a) i
_current = lens current $ flip update

_initial
  :: forall f ctx i e a. IsForm f ctx => FormContext ctx i => Lens' (f e a) i
_initial = lens initial $ flip load

_fstForm
  :: forall f ctx g e' a' e a
   . IsForm f ctx
  => Lens' (ProductForm (f e' a') g e a) (f e' a')
_fstForm = lens getFstForm $ flip setFstForm

_sndForm
  :: forall f g ctx e' a' e a
   . IsForm g ctx
  => Lens' (ProductForm f (g e' a') e a) (g e' a')
_sndForm = lens getSndForm $ flip setSndForm

_leftForm
  :: forall f ctx g e' a' e a
   . IsForm f ctx
  => Lens' (CoproductForm (f e' a') g e a) (f e' a')
_leftForm = lens getLeftForm $ flip setLeftForm

_rightForm
  :: forall f g ctx e' a' e a
   . IsForm g ctx
  => Lens' (CoproductForm f (g e' a') e a) (g e' a')
_rightForm = lens getRightForm $ flip setRightForm

propForm
  :: forall label rf rf' f ctx e' a' e a
   . IsForm f ctx
  => IsSymbol label
  => Row.Cons label (f e' a') rf' rf
  => Row.Lacks label rf'
  => Proxy label
  -> Lens' (RecordForm rf e a) (f e' a')
propForm prop = lens (getPropForm prop) $ flip $ setPropForm prop

output :: forall r f ctx e a. IsForm f ctx => Monoid r => Fold' r (f e a) a
output = to result <<< _Ok

error :: forall r f ctx e a. IsForm f ctx => Monoid r => Fold' r (f e a) e
error = to result <<< _Error
