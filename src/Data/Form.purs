module Data.Form
  ( FormT(..)
  , arbitraryForm
  , arbitraryFn
  , bindResult
  , class Form
  , collectResults
  , current
  , dirty
  , extendResult
  , form
  , form'
  , fromForm
  , imapInput
  , initial
  , ignoreError
  , load
  , loadFrom
  , mapForm
  , mapResult
  , peekResult
  , peeksResult
  , required
  , result
  , runForm
  , save
  , toForm
  , update
  , updateTo
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extend, extract)
import Control.Comonad.Env (EnvT(..), ask, runEnvT)
import Control.Comonad.Store
  ( Store
  , StoreT(..)
  , experiment
  , peek
  , peeks
  , pos
  , seek
  , seeks
  , store
  )
import Control.Monad.Gen (frequency, resize, sized)
import Data.Bifoldable
  ( class Bifoldable
  , bifoldMap
  , bifoldlDefault
  , bifoldrDefault
  )
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (note)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Form.Result (Result(..), fromEither, ignore)
import Data.Form.Result as R
import Data.Function (on)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (fst, uncurry)
import Data.Tuple.Nested ((/\))
import Test.QuickCheck
  ( class Arbitrary
  , class Coarbitrary
  , arbitrary
  , coarbitrary
  )
import Test.QuickCheck.Gen (Gen, runGen, stateful)
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype FormT i e a = FormT (EnvT i (Store i) (Result e a))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericForm :: Generic (FormT i e a) _

derive instance newtypeForm :: Newtype (FormT i e a) _

derive instance functorForm :: Functor (FormT i e)

instance bifunctorForm :: Bifunctor (FormT i) where
  bimap f g = mapResult $ bimap f g

instance invariantForm :: Invariant (FormT i e) where
  imap = imapF

instance foldableForm :: Foldable (FormT i e) where
  foldMap m = foldMap m <<< result
  foldr m = foldrDefault m
  foldl m = foldlDefault m

instance bifoldableForm :: Bifoldable (FormT i) where
  bifoldMap f g = bifoldMap f g <<< result
  bifoldr f = bifoldrDefault f
  bifoldl f = bifoldlDefault f

instance eqForm :: (Eq i, Eq (Result e a), Eq a) => Eq (FormT i e a) where
  eq = on eq $ map (pos &&& extract) <<< runEnvT <<< runForm

class Form f i | f -> i where
  toForm :: forall e a. f e a -> FormT i e a
  fromForm :: forall e a. FormT i e a -> f e a

mapForm
  :: forall f g i j e e' a b
   . Form f i
  => Form g j
  => (EnvT i (Store i) (Result e a) -> EnvT j (Store j) (Result e' b))
  -> f e a
  -> g e' b
mapForm f = fromForm <<< over FormT f <<< toForm

instance formForm :: Form (FormT i) i where
  toForm = identity
  fromForm = identity
else instance formNewtypeForm ::
  ( Newtype (f e a) (FormT i e a)
  ) =>
  Form f i where
  toForm = unsafeCoerce
  fromForm = unsafeCoerce

instance coarbitraryForm ::
  Coarbitrary (Result e a) =>
  Coarbitrary (FormT i e a) where
  coarbitrary = coarbitrary <<< result

instance showForm :: (Show (Result e a), Show i, Show a) => Show (FormT i e a) where
  show f =
    "(FormT "
      <> show (initial f)
      <> " "
      <> show (current f)
      <> " "
      <> show (result f)
      <> ")"

---------------------------------------------------------------------------------
---- Constructors
---------------------------------------------------------------------------------

form :: forall f i e a. Form f i => i -> (i -> Result e a) -> f e a
form i validate = fromForm $ FormT $ EnvT $ i /\ store validate i

form' :: forall f i e. Form f i => i -> f e i
form' i = form i pure

arbitraryForm
  :: forall f i e a
   . Form f i
  => Arbitrary i
  => Arbitrary a
  => Arbitrary e
  => Coarbitrary i
  => Gen (f e a)
  -> Gen (f e a)
arbitraryForm initialForm = sized \size -> do
  let
    updatedForm =
      update <$> arbitrary <*> (arbitraryForm initialForm)
        <|> load <$> arbitrary <*> (arbitraryForm initialForm)
  frequency
    $ 2.5 /\ initialForm :| [ toNumber size /\ resize (_ / 2) updatedForm ]

arbitraryFn :: forall b a. Coarbitrary a => Arbitrary b => Gen (a -> b)
arbitraryFn =
  stateful \s -> pure \o -> fst $ runGen (coarbitrary o arbitrary) s

---------------------------------------------------------------------------------
---- Combinators
---------------------------------------------------------------------------------

imapInput
  :: forall f g i j e
   . Form f i
  => Form g j
  => (j -> i)
  -> (i -> j)
  -> f e ~> g e
imapInput f g =
  mapForm \(EnvT (init /\ StoreT (Identity validator /\ curr))) ->
    EnvT $ g init /\ store (lcmap f validator) (g curr)

extendResult
  :: forall f i e e' a b
   . Form f i
  => (f e a -> Result e' b)
  -> f e a
  -> f e' b
extendResult f = mapForm $ extend $ f <<< fromForm <<< FormT

mapResult
  :: forall f i e e' a b
   . Form f i
  => (Result e a -> Result e' b)
  -> f e a
  -> f e' b
mapResult f = extendResult $ f <<< result

bindResult :: forall f i e a b. Form f i => (a -> Result e b) -> f e a -> f e b
bindResult f = extendResult $ f <=< result

required :: forall f i e a. Form f i => f e (Maybe a) -> f (Maybe e) a
required =
  mapResult
    $ R.result Unevaluated (Error <<< Just) (fromEither <<< note Nothing)

load :: forall f i e a. Form f i => i -> f e a -> f e a
load i =
  mapForm \(EnvT (_ /\ StoreT (Identity validator /\ _))) ->
    EnvT $ i /\ store validator i

loadFrom :: forall f i e a. Form f i => (i -> i) -> f e a -> f e a
loadFrom f = uncurry load <<< (f <<< initial &&& identity)

update :: forall f i e a. Form f i => i -> f e a -> f e a
update = mapForm <<< seek

updateTo :: forall f i e a. Form f i => (i -> i) -> f e a -> f e a
updateTo = mapForm <<< seeks

save :: forall f i e a. Form f i => f e a -> f e a
save = uncurry load <<< (current &&& identity)

---------------------------------------------------------------------------------
---- Eliminators
---------------------------------------------------------------------------------

runForm
  :: forall f i e a. Form f i => f e a -> EnvT i (Store i) (Result e a)
runForm = unwrap <<< toForm

current :: forall f i e a. Form f i => f e a -> i
current = pos <<< runForm

initial :: forall f i e a. Form f i => f e a -> i
initial = ask <<< runForm

result :: forall f i e a. Form f i => f e a -> Result e a
result = extract <<< runForm

ignoreError :: forall f i e e' a. Form f i => f e a -> Result e' a
ignoreError = ignore <<< result

peekResult :: forall f i e a. Form f i => i -> f e a -> Result e a
peekResult i = peek i <<< runForm

peeksResult :: forall f i e a. Form f i => (i -> i) -> f e a -> Result e a
peeksResult f = peeks f <<< runForm

collectResults
  :: forall f form i e a
   . Functor f
  => Form form i
  => (i -> f i)
  -> form e a
  -> f (Result e a)
collectResults f = experiment f <<< runForm

dirty :: forall f i e a. Form f i => Eq i => f e a -> Boolean
dirty = uncurry notEq <<< (initial &&& current)
