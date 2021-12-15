module Data.Form.Record
  ( Clear
  , RecordForm(..)
  , Current
  , Dirty
  , SetInput(..)
  , RecordContext
  , ValidateProp
  , getPropForm
  , record
  , recordValidate
  , setPropForm
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Eq (class EqRecord)
import Data.Form
  ( class FormContext
  , Form
  , clear
  , current
  , dirty
  , formValidate
  , getContext
  , ignoreError
  , load
  , overContext
  , update
  )
import Data.Form.Result (Result(..), fromEither)
import Data.Show (class ShowRecordFields)
import Data.Symbol (class IsSymbol)
import Heterogeneous.Folding
  ( class Folding
  , class FoldingWithIndex
  , class FoldlRecord
  , ConstFolding
  , hfoldl
  , hfoldlWithIndex
  )
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype RecordContext r = RC { | r }

unRC :: forall r. RecordContext r -> { | r }
unRC (RC t) = t

overRC
  :: forall r r'. ({ | r } -> { | r' }) -> RecordContext r -> RecordContext r'
overRC f (RC t) = RC $ f t

type RecordForm rf = Form (RecordContext rf)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

recordValidate
  :: forall rl rf e r a
   . RowToList rf rl
  => FoldlRecord ValidateProp (Result e {}) rl rf (Result e { | r })
  => { | rf }
  -> ({ | r } -> Either e a)
  -> RecordForm rf e a
recordValidate rf validate =
  formValidate (RC rf) $ fromEither <<< validate <=< validateRecord <<< unRC
  where
  validateRecord = hfoldlWithIndex ValidateProp (Ok {} :: Result e {})

record
  :: forall rl rf e r
   . RowToList rf rl
  => FoldlRecord ValidateProp (Result e {}) rl rf (Result e { | r })
  => { | rf }
  -> RecordForm rf e { | r }
record rf = recordValidate rf Right

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance eqRecordContext ::
  ( RowToList rf rl
  , EqRecord rl rf
  ) =>
  Eq (RecordContext rf)

derive newtype instance showRecordContext ::
  ( RowToList rf rl
  , ShowRecordFields rl rf
  ) =>
  Show (RecordContext rf)

data ValidateProp = ValidateProp

instance foldingWithIndexValidateProp ::
  ( IsSymbol label
  , Row.Lacks label r'
  , Row.Cons label a r' r
  ) =>
  FoldingWithIndex ValidateProp
    (Proxy label)
    (Result e { | r' })
    (Form ctx e' a)
    (Result e { | r }) where
  foldingWithIndex ValidateProp label acc f =
    Record.insert label <$> ignoreError f <*> acc

data Clear = Clear

instance mappingClear ::
  ( FormContext ctx i
  ) =>
  Mapping Clear (Form ctx e a) (Form ctx e a) where
  mapping Clear = clear

data Current = Current

instance mappingCurrent ::
  ( FormContext ctx i
  ) =>
  Mapping Current (Form ctx e a) i where
  mapping Current = current

data SetInput = Load | Update

instance foldingWithIndexSetInput ::
  ( IsSymbol label
  , FormContext ctx i
  , Row.Cons label (Form ctx e a) rf' rf
  ) =>
  FoldingWithIndex
    SetInput
    (Proxy label)
    (Builder { | rf } { | rf })
    i
    (Builder { | rf } { | rf }) where
  foldingWithIndex Load label builder i =
    Builder.modify label (load i) <<< builder
  foldingWithIndex Update label builder i =
    Builder.modify label (update i) <<< builder

data Dirty = Dirty

instance foldingDirty ::
  ( FormContext ctx i
  ) =>
  Folding Dirty Boolean ctx Boolean where
  folding Dirty true = const true
  folding Dirty _ = dirty

instance formContextRecordContext ::
  ( RowToList rf rlf
  , RowToList ri rli
  , HMap Clear { | rf } { | rf }
  , HMap Current { | rf } { | ri }
  , FoldlRecord (ConstFolding Dirty) Boolean rlf rf Boolean
  , FoldlRecord
      SetInput
      (Builder { | rf } { | rf })
      rli
      ri
      (Builder { | rf } { | rf })
  ) =>
  FormContext (RecordContext rf) { | ri } where
  clear = overRC $ hmap Clear
  current = hmap Current <<< unRC
  dirty = hfoldl Dirty false <<< unRC
  load = overRC
    <<< Builder.build
    <<< hfoldlWithIndex Load (identity :: Builder { | rf } { | rf })
  update = overRC
    <<< Builder.build
    <<< hfoldlWithIndex Update (identity :: Builder { | rf } { | rf })

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

getPropForm
  :: forall label rf rf' f e a
   . IsSymbol label
  => Row.Cons label f rf' rf
  => Row.Lacks label rf'
  => Proxy label
  -> RecordForm rf e a
  -> f
getPropForm label = Record.get label <<< unRC <<< getContext

setPropForm
  :: forall label rf rf' f e a
   . IsSymbol label
  => Row.Cons label f rf' rf
  => Row.Lacks label rf'
  => Proxy label
  -> f
  -> RecordForm rf e a
  -> RecordForm rf e a
setPropForm label = overContext <<< overRC <<< Record.set label
