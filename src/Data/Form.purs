module Data.Form
  ( Form
  , bindResult
  , class FormContext
  , clear
  , collectResults
  , current
  , dirty
  , extendResult
  , form'
  , formValidate
  , getContext
  , ignoreError
  , imapContext
  , load
  , mapResult
  , overContext
  , overForm
  , peekResult
  , peeksResult
  , required
  , result
  , save
  , setContext
  , update
  , updates
  ) where

import Prelude

import Control.Comonad (extend, extract)
import Control.Comonad.Store
  ( Store
  , StoreT(..)
  , experiment
  , peeks
  , pos
  , seek
  , seeks
  , store
  )
import Control.Semigroupoid (composeFlipped)
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
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Strong (second, (&&&))
import Data.Tuple (snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Coarbitrary, coarbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype Form ctx e a = Form (Store ctx (Result e a))

overForm
  :: forall ctx ctx' e e' a b
   . (Store ctx (Result e a) -> Store ctx' (Result e' b))
  -> Form ctx e a
  -> Form ctx' e' b
overForm f = Form <<< f <<< runForm

runForm :: forall ctx e a. Form ctx e a -> Store ctx (Result e a)
runForm (Form s) = s

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

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
  eq = on eq $ getContext &&& result

class FormContext ctx i | ctx -> i where
  clear :: ctx -> ctx
  current :: ctx -> i
  dirty :: ctx -> Boolean
  load :: i -> ctx -> ctx
  update :: i -> ctx -> ctx

instance formContextTuple :: (Eq i, Monoid i) => FormContext (i /\ i) i where
  clear = second $ const mempty
  current = snd
  dirty = uncurry (/=)
  load = const <<< (identity &&& identity)
  update = second <<< const

instance formContextForm :: FormContext ctx i => FormContext (Form ctx e a) i where
  clear = overContext clear
  current = current <<< getContext
  dirty = dirty <<< getContext
  load = overContext <<< load
  update = overContext <<< update

updates :: forall i ctx. FormContext ctx i => (i -> i) -> ctx -> ctx
updates f fm = update (f $ current fm) fm

save :: forall ctx i e a. FormContext ctx i => Form ctx e a -> Form ctx e a
save = uncurry load <<< (current &&& identity)

-- class FormContext ctx i <= IndexedContext index ctx i | ctx -> i where

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
      <> show (pos $ runForm f)
      <> " "
      <> show (result f)
      <> ")"

---------------------------------------------------------------------------------
---- Constructors
---------------------------------------------------------------------------------

formValidate :: forall ctx e a. ctx -> (ctx -> Result e a) -> Form ctx e a
formValidate ctx validate = Form $ store validate ctx

form' :: forall ctx e. ctx -> Form ctx e ctx
form' ctx = formValidate ctx pure

-- arbitraryForm
--   :: forall ctx i o e a
--    . FormContext ctx i
--   => Arbitrary ctx
--   => Arbitrary i
--   => Arbitrary a
--   => Arbitrary e
--   => Coarbitrary o
--   => (ctx -> (o -> Either e a) -> Form ctx e a)
--   -> Gen (Form ctx e a)
-- arbitraryForm mkForm = sized \size -> do
--   let
--     blankForm =
--       mkForm
--         <$> map clearInput arbitrary
--         <*> stateful \s -> pure \o -> fst $ runGen (coarbitrary o arbitrary) s
--     updatedForm =
--       update <$> arbitrary <*> (arbitraryForm mkForm)
--         <|> load <$> arbitrary <*> (arbitraryForm mkForm)
--   frequency
--     $ 2.5 /\ blankForm :| [ toNumber size /\ resize (_ / 2) updatedForm ]

---------------------------------------------------------------------------------
---- Combinators
---------------------------------------------------------------------------------

imapContext
  :: forall ctx ctx' e
   . (ctx' -> ctx)
  -> (ctx -> ctx')
  -> Form ctx e ~> Form ctx' e
imapContext f g =
  overForm \(StoreT (Identity validator /\ ctx)) ->
    store (lcmap f validator) (g ctx)

extendResult
  :: forall ctx e e' a b
   . (Form ctx e a -> Result e' b)
  -> Form ctx e a
  -> Form ctx e' b
extendResult = overForm <<< extend <<< composeFlipped Form

mapResult
  :: forall ctx e e' a b
   . (Result e a -> Result e' b)
  -> Form ctx e a
  -> Form ctx e' b
mapResult f = extendResult $ f <<< result

bindResult
  :: forall ctx e a b. (a -> Result e b) -> Form ctx e a -> Form ctx e b
bindResult f = extendResult $ f <=< result

required :: forall ctx e a. Form ctx e (Maybe a) -> Form ctx (Maybe e) a
required =
  mapResult
    $ R.result Unevaluated (Error <<< Just) (fromEither <<< note Nothing)

overContext :: forall ctx e a. (ctx -> ctx) -> Form ctx e a -> Form ctx e a
overContext = overForm <<< seeks

setContext :: forall ctx e a. ctx -> Form ctx e a -> Form ctx e a
setContext = overForm <<< seek

---------------------------------------------------------------------------------
---- Eliminators
---------------------------------------------------------------------------------

getContext :: forall ctx e a. Form ctx e a -> ctx
getContext = pos <<< runForm

result :: forall ctx e a. Form ctx e a -> Result e a
result = extract <<< runForm

ignoreError :: forall ctx e e' a. Form ctx e a -> Result e' a
ignoreError = ignore <<< result

peekResult
  :: forall i ctx e a. FormContext ctx i => i -> Form ctx e a -> Result e a
peekResult i = peeks (update i) <<< runForm

peeksResult
  :: forall i ctx e a
   . FormContext ctx i
  => (i -> i)
  -> Form ctx e a
  -> Result e a
peeksResult f = peeks (updates f) <<< runForm

collectResults
  :: forall ctx i e f a
   . Functor f
  => FormContext ctx i
  => (i -> f i)
  -> Form ctx e a
  -> f (Result e a)
collectResults f = experiment f' <<< runForm
  where
  f' ctx = flip update ctx <$> f (current ctx)
