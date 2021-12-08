module Data.Form.Context where

import Prelude

class FormContext ctx i o | ctx -> i o where
  blank :: ctx
  current :: ctx -> i
  initial :: ctx -> i
  load :: i -> ctx -> ctx
  output :: ctx -> o
  update :: i -> ctx -> ctx

dirty :: forall ctx i o. Eq i => FormContext ctx i o => ctx -> Boolean
dirty ctx = initial ctx /= current ctx

save :: forall ctx i o. FormContext ctx i o => ctx -> ctx
save ctx = load (current ctx) ctx
