module Data.Form
  ( Current
  , Form
  , FormF
  , FormT(..)
  , FormV
  , ProformT(..)
  , class FormContext
  , class Requireable
  , class ToStar
  , class UpdateRecord
  , current
  , extendF
  , extractF
  , form
  , formEmpty
  , formEmptyF
  , formEmptyV
  , formF
  , formV
  , hoistFormP
  , hoistFormW
  , mapFormT
  , peekF
  , peeksF
  , require
  , required
  , requiredV
  , runForm
  , runFormF
  , runFormT
  , seekForm
  , seeksForm
  , toStar
  , update
  , updateRecord
  , updates
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Comonad (class Comonad, class Extend, extract, (=>>))
import Control.Comonad.Store (class ComonadStore)
import Control.Monad.Error.Class (throwError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NA
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Form.Required (Required(..))
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Profunctor (class Profunctor, arr)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong ((***))
import Data.Set (Set)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NSet
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NS
import Data.Symbol (class IsSymbol)
import Data.These (These, both)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
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

data FormT :: (Type -> Type -> Type) -> Type -> (Type -> Type) -> Type -> Type
data FormT p s w a = FormT (w (p s a)) s

newtype ProformT p w s a = ProformT (FormT p s w a)

type Form s = FormT (->) s Identity
type FormF f s = FormT (Star f) s Identity
type FormV e s a = FormF (Either e) s a

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance newtypeFormT :: Newtype (ProformT p w s a) _

derive instance functorFormT ::
  ( Functor w
  , Functor (p s)
  ) =>
  Functor (FormT p s w)

derive instance functorProformT ::
  ( Functor w
  , Functor (p s)
  ) =>
  Functor (ProformT p w s)

instance invariantFormT :: (Functor w, Functor (p s)) => Invariant (FormT p s w) where
  imap = imapF

derive newtype instance invariantProformT ::
  ( Functor w
  , Functor (p s)
  ) =>
  Invariant (ProformT p w s)

instance extendFormT ::
  ( Category p
  , Functor (p s)
  , Profunctor p
  , Extend w
  ) =>
  Extend (FormT p s w) where
  extend f (FormT w s) = FormT (w =>> (\w' -> arr $ f <<< FormT w')) s

instance comonadFormT :: Comonad w => Comonad (FormT (->) s w) where
  extract (FormT w s) = extract w s

instance comonadStoreFormT :: Comonad w => ComonadStore s (FormT (->) s w) where
  pos (FormT _ s) = s
  peek s (FormT w _) = extract w s

instance semigroupoidProformT ::
  ( Semigroupoid p
  , Comonad w
  , Bind w
  ) =>
  Semigroupoid (ProformT p w) where
  compose (ProformT (FormT w _)) (ProformT (FormT x s)) =
    ProformT $ FormT
      (join $ x =>> \x' -> w =>> \w' -> extract w' <<< extract x')
      s

class FormContext s i | s -> i where
  current :: s -> i
  update :: i -> s -> s

instance formContextIdentity :: FormContext (Identity a) a where
  current = unwrap
  update = const <<< Identity

instance formContextForm :: FormContext s i => FormContext (FormT p s w a) i where
  current (FormT _ s) = current s
  update i (FormT w s) = FormT w $ update i s

instance formContextTuple ::
  ( FormContext s i
  , FormContext t j
  ) =>
  FormContext (s /\ t) (i /\ j) where
  current = current *** current
  update (i /\ j) = update i *** update j

data Current = Current

instance mappingCurrent :: FormContext s i => Mapping Current s i where
  mapping Current = current

instance formContextRecord ::
  ( RowToList ri rli
  , RowToList rs rls
  , UpdateRecord rli rls ri rs
  , HMap Current { | rs } { | ri }
  ) =>
  FormContext { | rs } { | ri } where
  current = hmap Current
  update = Builder.build <<< updateRecord (Proxy :: _ rli) (Proxy :: _ rls)

class UpdateRecord
  :: RowList Type -> RowList Type -> Row Type -> Row Type -> Constraint
class UpdateRecord rli rls ri rs | rli -> ri, rls -> rs where
  updateRecord
    :: Proxy rli -> Proxy rls -> { | ri } -> Builder { | rs } { | rs }

instance updateRecordNil :: UpdateRecord RL.Nil RL.Nil ri rs where
  updateRecord _ _ _ = identity

instance updateRecordCons ::
  ( IsSymbol lbl
  , UpdateRecord rli rls ri rs
  , Row.Cons lbl i ri' ri
  , Row.Cons lbl s rs' rs
  , FormContext s i
  ) =>
  UpdateRecord (RL.Cons lbl i rli) (RL.Cons lbl s rls) ri rs where
  updateRecord _ _ ri =
    Builder.modify prop (update $ Record.get prop ri)
      <<< updateRecord (Proxy :: _ rli) (Proxy :: _ rls) ri
    where
    prop = Proxy :: _ lbl

updates :: forall s i. FormContext s i => (i -> i) -> s -> s
updates f s = update (f $ current s) s

---------------------------------------------------------------------------------
---- Constructors
---------------------------------------------------------------------------------

form :: forall s a. (s -> a) -> s -> Form s a
form = FormT <<< Identity

formEmpty :: forall s a. Monoid s => (s -> a) -> Form s a
formEmpty p = form p mempty

formF :: forall f s a. (s -> f a) -> s -> FormF f s a
formF = FormT <<< Identity <<< Star

formEmptyF :: forall f s a. Monoid s => (s -> f a) -> FormF f s a
formEmptyF p = formF p mempty

formV :: forall e s a. (s -> Either e a) -> s -> FormV e s a
formV = FormT <<< Identity <<< Star

formEmptyV :: forall e s a. Monoid s => (s -> Either e a) -> FormV e s a
formEmptyV p = formV p mempty

---------------------------------------------------------------------------------
---- Combinators
---------------------------------------------------------------------------------

class ToStar :: (Type -> Type -> Type) -> (Type -> Type) -> Constraint
class ToStar p f | p -> f where
  toStar :: forall s. p s ~> Star f s

instance toStarFunction :: Applicative f => ToStar (->) f where
  toStar = Star <<< map pure

instance toStarStar :: ToStar (Star f) f where
  toStar = identity

class Requireable a' a | a' -> a where
  require :: forall e. a' -> Either (Required e) a

instance requireableIdentity :: Requireable (Identity a) a where
  require = pure <<< unwrap

instance requireableMaybe :: Requireable (Maybe a) a where
  require Nothing = throwError Absent
  require (Just a) = pure a

instance requireableString :: Requireable String NonEmptyString where
  require = require <<< NS.fromString

instance requireableArray :: Requireable (Array a) (NonEmptyArray a) where
  require = require <<< NA.fromArray

instance requireableList :: Requireable (List a) (NonEmptyList a) where
  require = require <<< NL.fromList

instance requireableSet :: Requireable (Set a) (NonEmptySet a) where
  require = require <<< NSet.fromSet

instance requireableThese :: Requireable (These a b) (Tuple a b) where
  require = require <<< both

mapFormT
  :: forall p q s w x a b
   . (w (p s a) -> x (q s b))
  -> FormT p s w a
  -> FormT q s x b
mapFormT t (FormT w a) = FormT (t w) a

hoistFormW
  :: forall p s w x
   . w ~> x
  -> FormT p s w ~> FormT p s x
hoistFormW = mapFormT

hoistFormP
  :: forall p q s w
   . Functor w
  => p s ~> q s
  -> FormT p s w ~> FormT q s w
hoistFormP t = mapFormT $ map t

liftF
  :: forall f s w a. Functor w => FormT (->) s w (f a) -> FormT (Star f) s w a
liftF = mapFormT $ map Star

required
  :: forall s e w a' a
   . Functor w
  => Requireable a' a
  => FormT (->) s w a'
  -> FormT (Star (Either (Required e))) s w a
required = liftF <<< map require

requiredV
  :: forall s e w a' a
   . Functor w
  => Requireable a' a
  => FormT (Star (Either e)) s w a'
  -> FormT (Star (Either (Required e))) s w a
requiredV =
  mapFormT
    $ map
    $ over Star (map (bindFlipped require <<< lmap Present)) <<< toStar

runForm :: forall s a. Form s a -> Tuple (s -> a) s
runForm (FormT (Identity f) s) = Tuple f s

runFormF :: forall f s a. FormF f s a -> Tuple (s -> f a) s
runFormF (FormT (Identity (Star f)) s) = Tuple f s

runFormT :: forall p s w a. FormT p s w a -> Tuple (w (p s a)) s
runFormT (FormT w s) = Tuple w s

extendF :: forall g f s a b. (FormF f s a -> g b) -> FormF f s a -> FormF g s b
extendF k (FormT (Identity (Star f)) s) = FormT
  (Identity $ Star $ k <<< formF f)
  s

extractF :: forall s f a. FormF f s a -> f a
extractF (FormT (Identity (Star f)) s) = f s

peekF :: forall s f a. s -> FormF f s a -> f a
peekF s (FormT (Identity (Star f)) _) = f s

peeksF :: forall s f a. (s -> s) -> FormF f s a -> f a
peeksF k (FormT (Identity (Star f)) s) = f $ k s

seekForm :: forall p s w a. s -> FormT p s w a -> FormT p s w a
seekForm s (FormT w _) = FormT w s

seeksForm :: forall p s w a. (s -> s) -> FormT p s w a -> FormT p s w a
seeksForm f (FormT w s) = FormT w $ f s
