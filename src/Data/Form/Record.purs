module Data.Form.Record
  ( Extract
  , RecordForm
  , RecordFormF
  , RecordFormV
  , class ExtractFRecord
  , extractFRecord
  , record
  , recordF
  , recordV
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Form (Form, FormF, FormV, extractF, form, formF)
import Data.Symbol (class IsSymbol)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data Extract = Extract

type RecordForm rs = Form { | rs }
type RecordFormF f rs a = FormF f { | rs } a
type RecordFormV e rs a = FormV e { | rs } a

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

record
  :: forall rs ra
   . HMap Extract { | rs } { | ra }
  => { | rs }
  -> RecordForm rs { | ra }
record = form $ hmap Extract

recordF
  :: forall rl f rs ra
   . Functor f
  => RowToList rs rl
  => ExtractFRecord rl f rs ra
  => { | rs }
  -> RecordFormF f rs { | ra }
recordF rs =
  formF
    (map (flip Builder.build {}) <<< extractFRecord (Proxy :: _ rl))
    rs

recordV
  :: forall rl rs ra
   . RowToList rs rl
  => ExtractFRecord rl (Either Unit) rs ra
  => { | rs }
  -> RecordFormV Unit rs { | ra }
recordV = recordF

instance mappingExtract :: Comonad w => Mapping Extract (w a) a where
  mapping Extract = extract

class ExtractFRecord (rl :: RowList Type) f rs ra | rl -> f rs ra where
  extractFRecord :: Proxy rl -> { | rs } -> f (Builder {} { | ra })

instance extractFRecordNil :: Applicative f => ExtractFRecord RL.Nil f rs () where
  extractFRecord _ _ = pure identity

instance extractFRecordConsEither ::
  ( IsSymbol lbl
  , ExtractFRecord rl (Either Unit) rs ra'
  , Row.Cons lbl (FormV e s a) rs' rs
  , Row.Cons lbl a ra' ra
  , Row.Lacks lbl ra'
  ) =>
  ExtractFRecord (RL.Cons lbl (FormV e s a) rl) (Either Unit) rs ra where
  extractFRecord _ rs =
    compose
      <$>
        ( map (Builder.insert prop)
            $ lmap (const unit)
            $ extractF
            $ Record.get prop rs
        )
      <*> extractFRecord (Proxy :: _ rl) rs
    where
    prop = Proxy :: _ lbl

else instance extractFRecordCons ::
  ( Apply f
  , IsSymbol lbl
  , ExtractFRecord rl f rs ra'
  , Row.Cons lbl (FormF f s a) rs' rs
  , Row.Cons lbl a ra' ra
  , Row.Lacks lbl ra'
  ) =>
  ExtractFRecord (RL.Cons lbl (FormF f s a) rl) f rs ra where
  extractFRecord _ rs =
    compose
      <$> (map (Builder.insert prop) $ extractF $ Record.get prop rs)
      <*> extractFRecord (Proxy :: _ rl) rs
    where
    prop = Proxy :: _ lbl
