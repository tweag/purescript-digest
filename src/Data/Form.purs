module Data.Form
  ( Form(..)
  , arbitraryForm
  , bindResult
  , class FormContext
  , class IsForm
  , clear
  , clearInput
  , collectResults
  , current
  , currentContext
  , dirty
  , extendResult
  , form'
  , formValidate
  , fromForm
  , getInput
  , ignoreError
  , imapContext
  , initial
  , initialContext
  , load
  , loadContext
  , loads
  , loadsContext
  , mapForm
  , mapInput
  , mapResult
  , peekResult
  , peekResultContext
  , peeksResult
  , peeksResultContext
  , required
  , result
  , runForm
  , save
  , setInput
  , toForm
  , update
  , updateContext
  , updates
  , updatesContext
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
import Data.Either (Either, note)
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

newtype Form ctx e a = Form (EnvT ctx (Store ctx) (Result e a))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericForm :: Generic (Form ctx e a) _

derive instance newtypeForm :: Newtype (Form ctx e a) _

derive instance functorForm :: Functor (Form ctx e)

instance bifunctorForm :: Bifunctor (Form ctx) where
  bimap f g = mapResult $ bimap f g

instance invariantForm :: Invariant (Form ctx e) where
  imap = imapF

instance foldableForm :: Foldable (Form ctx e) where
  foldMap m = foldMap m <<< result
  foldr m = foldrDefault m
  foldl m = foldlDefault m

instance bifoldableForm :: Bifoldable (Form ctx) where
  bifoldMap f g = bifoldMap f g <<< result
  bifoldr f = bifoldrDefault f
  bifoldl f = bifoldlDefault f

instance eqForm :: (Eq ctx, Eq (Result e a), Eq a) => Eq (Form ctx e a) where
  eq = on eq $ map (pos &&& extract) <<< runEnvT <<< runForm

class IsForm f ctx | f -> ctx where
  toForm :: forall e a. f e a -> Form ctx e a
  fromForm :: forall e a. Form ctx e a -> f e a

class FormContext ctx i | ctx -> i where
  clearInput :: ctx -> ctx
  getInput :: ctx -> i
  setInput :: i -> ctx -> ctx

mapInput :: forall ctx i. FormContext ctx i => (i -> i) -> ctx -> ctx
mapInput f ctx = setInput (f $ getInput ctx) ctx

mapForm
  :: forall f g ctx ctx' e e' a b
   . IsForm f ctx
  => IsForm g ctx'
  => (EnvT ctx (Store ctx) (Result e a) -> EnvT ctx' (Store ctx') (Result e' b))
  -> f e a
  -> g e' b
mapForm f = fromForm <<< over Form f <<< toForm

instance formForm :: IsForm (Form ctx) ctx where
  toForm = identity
  fromForm = identity
else instance formNewtypeForm ::
  ( Newtype (f e a) (f' e a)
  , IsForm f' ctx
  ) =>
  IsForm f ctx where
  toForm = unsafeCoerce
  fromForm = unsafeCoerce

instance formContextIdentity :: Monoid ctx => FormContext (Identity ctx) ctx where
  clearInput = const mempty
  getInput = unwrap
  setInput = const <<< Identity

instance coarbitraryForm ::
  Coarbitrary (Result e a) =>
  Coarbitrary (Form ctx e a) where
  coarbitrary = coarbitrary <<< result

instance showForm ::
  ( Show (Result e a)
  , Show ctx
  , Show a
  ) =>
  Show (Form ctx e a) where
  show f =
    "(Form "
      <> show (initialContext f)
      <> " "
      <> show (currentContext f)
      <> " "
      <> show (result f)
      <> ")"

---------------------------------------------------------------------------------
---- Constructors
---------------------------------------------------------------------------------

formValidate
  :: forall f ctx e a. IsForm f ctx => ctx -> (ctx -> Result e a) -> f e a
formValidate ctx validate = fromForm $ Form $ EnvT $ ctx /\ store validate ctx

form' :: forall f ctx e. IsForm f ctx => ctx -> f e ctx
form' ctx = formValidate ctx pure

arbitraryForm
  :: forall f ctx i o e a
   . IsForm f ctx
  => FormContext ctx i
  => Arbitrary ctx
  => Arbitrary i
  => Arbitrary a
  => Arbitrary e
  => Coarbitrary o
  => (ctx -> (o -> Either e a) -> f e a)
  -> Gen (f e a)
arbitraryForm mkForm = sized \size -> do
  let
    blankForm =
      mkForm
        <$> map clearInput arbitrary
        <*> stateful \s -> pure \o -> fst $ runGen (coarbitrary o arbitrary) s
    updatedForm =
      update <$> arbitrary <*> (arbitraryForm mkForm)
        <|> load <$> arbitrary <*> (arbitraryForm mkForm)
  frequency
    $ 2.5 /\ blankForm :| [ toNumber size /\ resize (_ / 2) updatedForm ]

---------------------------------------------------------------------------------
---- Combinators
---------------------------------------------------------------------------------

imapContext
  :: forall f g ctx ctx' e
   . IsForm f ctx
  => IsForm g ctx'
  => (ctx' -> ctx)
  -> (ctx -> ctx')
  -> f e ~> g e
imapContext f g =
  mapForm \(EnvT (init /\ StoreT (Identity validator /\ curr))) ->
    EnvT $ g init /\ store (lcmap f validator) (g curr)

extendResult
  :: forall f ctx e e' a b
   . IsForm f ctx
  => (f e a -> Result e' b)
  -> f e a
  -> f e' b
extendResult f = mapForm $ extend $ f <<< fromForm <<< Form

mapResult
  :: forall f ctx e e' a b
   . IsForm f ctx
  => (Result e a -> Result e' b)
  -> f e a
  -> f e' b
mapResult f = extendResult $ f <<< result

bindResult
  :: forall f ctx e a b. IsForm f ctx => (a -> Result e b) -> f e a -> f e b
bindResult f = extendResult $ f <=< result

required :: forall f ctx e a. IsForm f ctx => f e (Maybe a) -> f (Maybe e) a
required =
  mapResult
    $ R.result Unevaluated (Error <<< Just) (fromEither <<< note Nothing)

loadContext :: forall f ctx e a. IsForm f ctx => ctx -> f e a -> f e a
loadContext i =
  mapForm \(EnvT (_ /\ StoreT (Identity validator /\ _))) ->
    EnvT $ i /\ store validator i

load
  :: forall i f ctx e a
   . IsForm f ctx
  => FormContext ctx i
  => i
  -> f e a
  -> f e a
load = loadsContext <<< setInput

loadsContext :: forall f ctx e a. IsForm f ctx => (ctx -> ctx) -> f e a -> f e a
loadsContext f = uncurry loadContext <<< (f <<< initialContext &&& identity)

loads
  :: forall i f ctx e a
   . IsForm f ctx
  => FormContext ctx i
  => (i -> i)
  -> f e a
  -> f e a
loads = loadsContext <<< mapInput

updateContext :: forall f ctx e a. IsForm f ctx => ctx -> f e a -> f e a
updateContext = mapForm <<< seek

update
  :: forall i f ctx e a
   . IsForm f ctx
  => FormContext ctx i
  => i
  -> f e a
  -> f e a
update = updatesContext <<< setInput

updatesContext
  :: forall f ctx e a. IsForm f ctx => (ctx -> ctx) -> f e a -> f e a
updatesContext = mapForm <<< seeks

updates
  :: forall i f ctx e a
   . IsForm f ctx
  => FormContext ctx i
  => (i -> i)
  -> f e a
  -> f e a
updates = updatesContext <<< mapInput

clear
  :: forall i f ctx e a
   . IsForm f ctx
  => FormContext ctx i
  => f e a
  -> f e a
clear = updatesContext clearInput

save :: forall f ctx e a. IsForm f ctx => f e a -> f e a
save = uncurry loadContext <<< (currentContext &&& identity)

---------------------------------------------------------------------------------
---- Eliminators
---------------------------------------------------------------------------------

runForm
  :: forall f ctx e a
   . IsForm f ctx
  => f e a
  -> EnvT ctx (Store ctx) (Result e a)
runForm = unwrap <<< toForm

current :: forall i f ctx e a. FormContext ctx i => IsForm f ctx => f e a -> i
current = getInput <<< currentContext

currentContext :: forall f ctx e a. IsForm f ctx => f e a -> ctx
currentContext = pos <<< runForm

initial :: forall i f ctx e a. FormContext ctx i => IsForm f ctx => f e a -> i
initial = getInput <<< initialContext

initialContext :: forall f ctx e a. IsForm f ctx => f e a -> ctx
initialContext = ask <<< runForm

result :: forall f ctx e a. IsForm f ctx => f e a -> Result e a
result = extract <<< runForm

ignoreError :: forall f ctx e e' a. IsForm f ctx => f e a -> Result e' a
ignoreError = ignore <<< result

peekResult
  :: forall i f ctx e a
   . FormContext ctx i
  => IsForm f ctx
  => i
  -> f e a
  -> Result e a
peekResult = peeksResultContext <<< setInput

peekResultContext
  :: forall f ctx e a. IsForm f ctx => ctx -> f e a -> Result e a
peekResultContext ctx = peek ctx <<< runForm

peeksResult
  :: forall i f ctx e a
   . FormContext ctx i
  => IsForm f ctx
  => (i -> i)
  -> f e a
  -> Result e a
peeksResult = peeksResultContext <<< mapInput

peeksResultContext
  :: forall f ctx e a. IsForm f ctx => (ctx -> ctx) -> f e a -> Result e a
peeksResultContext f = peeks f <<< runForm

collectResults
  :: forall i f form ctx e a
   . Functor f
  => IsForm form ctx
  => FormContext ctx i
  => (i -> f i)
  -> form e a
  -> f (Result e a)
collectResults f = experiment f' <<< runForm
  where
  f' ctx = flip setInput ctx <$> f (getInput ctx)

dirty
  :: forall i f ctx e a
   . FormContext ctx i
  => IsForm f ctx
  => Eq i
  => f e a
  -> Boolean
dirty = uncurry notEq <<< (initial &&& current)
