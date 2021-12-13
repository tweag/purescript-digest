module Data.Form.Record
  ( Clear
  , RecordForm(..)
  , Current
  , Load
  , RecordContext
  , ValidateProp
  , class CoarbitraryRecord
  , coarbitraryRecord
  , getPropForm
  , record
  , recordValidate
  , setPropForm
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Eq (class EqRecord)
import Data.Foldable (class Foldable)
import Data.Form
  ( class FormContext
  , class IsForm
  , Form(..)
  , arbitraryForm
  , clear
  , current
  , currentContext
  , formValidate
  , ignoreError
  , initialContext
  , load
  , loadContext
  , loadsContext
  , save
  , updateContext
  , updatesContext
  )
import Data.Form.Result (Result(..))
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show (class ShowRecordFields)
import Data.Symbol (class IsSymbol)
import Heterogeneous.Folding
  ( class FoldingWithIndex
  , class FoldlRecord
  , hfoldlWithIndex
  )
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Test.QuickCheck (class Arbitrary, class Coarbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen)
import Type.Prelude (Proxy(..))
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

newtype RecordForm rf e a = RecordForm (Form (RecordContext rf) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

recordValidate
  :: forall rl rf e r a
   . RowToList rf rl
  => FoldlRecord ValidateProp (Result e {}) rl rf (Result e { | r })
  => { | rf }
  -> ({ | r } -> Result e a)
  -> RecordForm rf e a
recordValidate rf validate =
  formValidate (RC rf) $ validate <=< validateRecord <<< unRC
  where
  validateRecord = hfoldlWithIndex ValidateProp (Ok {} :: Result e {})

record
  :: forall rl rf e r
   . RowToList rf rl
  => FoldlRecord ValidateProp (Result e {}) rl rf (Result e { | r })
  => { | rf }
  -> RecordForm rf e { | r }
record rf = recordValidate rf Ok

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericRecordForm :: Generic (RecordForm rf e a) _

derive instance newtypeRecordForm :: Newtype (RecordForm rf e a) _

derive instance eqRecordContext ::
  ( RowToList rf rl
  , EqRecord rl rf
  ) =>
  Eq (RecordContext rf)

derive instance eqRecordForm ::
  ( RowToList rf rl
  , EqRecord rl rf
  , Eq e
  , Eq a
  ) =>
  Eq (RecordForm rf e a)

derive instance functorRecordForm :: Functor (RecordForm rf e)

derive newtype instance invariantRecordForm ::
  Invariant (RecordForm rf e)

derive newtype instance bifunctorRecordForm ::
  Bifunctor (RecordForm rf)

derive newtype instance foldableRecordForm ::
  Foldable (RecordForm rf e)

derive newtype instance bifoldableRecordForm ::
  Bifoldable (RecordForm rf)

derive newtype instance showRecordContext ::
  ( RowToList rf rl
  , ShowRecordFields rl rf
  ) =>
  Show (RecordContext rf)

derive newtype instance showRecordForm ::
  ( RowToList rf rl
  , ShowRecordFields rl rf
  , Show e
  , Show a
  ) =>
  Show (RecordForm rf e a)

data ValidateProp = ValidateProp

instance foldingWithIndexValidateProp ::
  ( IsSymbol label
  , IsForm f ctx
  , Row.Lacks label r'
  , Row.Cons label a r' r
  ) =>
  FoldingWithIndex ValidateProp
    (Proxy label)
    (Result e { | r' })
    (f e' a)
    (Result e { | r }) where
  foldingWithIndex ValidateProp label acc f =
    Record.insert label <$> ignoreError f <*> acc

data Clear = Clear

instance mappingClear ::
  ( IsForm f ctx
  , FormContext ctx i
  ) =>
  Mapping Clear (f e a) (f e a) where
  mapping Clear = save <<< clear

data Current = Current

instance mappingCurrent ::
  ( IsForm f ctx
  , FormContext ctx i
  ) =>
  Mapping Current (f e a) i where
  mapping Current = current

data Load = Load

instance foldingWithIndexLoad ::
  ( IsSymbol label
  , IsForm f ctx
  , FormContext ctx i
  , Row.Cons label (f e a) rf' rf
  ) =>
  FoldingWithIndex
    Load
    (Proxy label)
    (Builder { | rf } { | rf })
    i
    (Builder { | rf } { | rf }) where
  foldingWithIndex Load label builder i =
    Builder.modify label (load i) <<< builder

instance formContextRecordContext ::
  ( RowToList rf rlf
  , RowToList ri rli
  , HMap Clear { | rf } { | rf }
  , HMap Current { | rf } { | ri }
  , FoldlRecord
      Load
      (Builder { | rf } { | rf })
      rli
      ri
      (Builder { | rf } { | rf })
  ) =>
  FormContext (RecordContext rf) { | ri } where
  clearInput = overRC $ hmap Clear
  getInput = hmap Current <<< unRC
  setInput =
    overRC
      <<< Builder.build
      <<< hfoldlWithIndex Load (identity :: Builder { | rf } { | rf })

derive newtype instance arbitraryRecordContext ::
  ( Arbitrary { | rf }
  ) =>
  Arbitrary (RecordContext rf)

instance coarbitraryRecordContext ::
  ( RowToList r rl
  , CoarbitraryRecord rl r
  ) =>
  Coarbitrary (RecordContext r) where
  coarbitrary = coarbitraryRecord (Proxy :: _ rl) <<< unRC

class CoarbitraryRecord (rl :: RowList Type) r | rl -> r where
  coarbitraryRecord :: forall a. Proxy rl -> { | r } -> Gen a -> Gen a

instance coarbitraryRecordNil :: CoarbitraryRecord RL.Nil r where
  coarbitraryRecord _ _ = identity

instance coarbitraryRecordCons ::
  ( IsSymbol label
  , Row.Cons label a r' r
  , CoarbitraryRecord rl r
  , Coarbitrary a
  ) =>
  CoarbitraryRecord (RL.Cons label a rl) r where
  coarbitraryRecord _ r =
    coarbitrary (Record.get (Proxy :: _ label) r)
      <<< coarbitraryRecord (Proxy :: _ rl) r

instance arbitraryRecordForm ::
  ( RowToList rf rl
  , Arbitrary { | rf }
  , FormContext (RecordContext rf) ri
  , FoldlRecord ValidateProp (Result e {}) rl rf (Result e { | r })
  , Arbitrary ri
  , Arbitrary e
  , Arbitrary a
  , Coarbitrary (RecordContext r)
  ) =>
  Arbitrary (RecordForm rf e a) where
  arbitrary = arbitraryForm recordValidate'
    where
    recordValidate' rc f = recordValidate (unRC rc) $ f <<< RC

derive newtype instance coarbitraryRecordForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (RecordForm rf e a)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

getPropForm
  :: forall label rf rf' f ctx e' a' e a
   . IsForm f ctx
  => IsSymbol label
  => Row.Cons label (f e' a') rf' rf
  => Row.Lacks label rf'
  => Proxy label
  -> RecordForm rf e a
  -> f e' a'
getPropForm label rf = updateContext current' initial'
  where
  current' = currentContext $ Record.get label $ unRC $ currentContext rf
  initial' = Record.get label $ unRC $ initialContext rf

setPropForm
  :: forall label rf rf' f ctx e' a' e a
   . IsForm f ctx
  => IsSymbol label
  => Row.Cons label (f e' a') rf' rf
  => Row.Lacks label rf'
  => Proxy label
  -> f e' a'
  -> RecordForm rf e a
  -> RecordForm rf e a
setPropForm label f =
  updatesContext (go currentContext) <<< loadsContext (go initialContext)
  where
  go getter = overRC $ Record.modify label (loadContext $ getter f)
