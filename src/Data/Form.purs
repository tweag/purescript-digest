module Data.Form
  ( Form
  , arbitraryFormContext
  , class FormContext
  , class IsForm
  , collectResults
  , current
  , dirty
  , extendResult
  , extractResult
  , form
  , form'
  , fromForm
  , imapContext
  , initial
  , load
  , mapEither
  , mkForm
  , output
  , overContext
  , peekResult
  , peeksResult
  , required
  , runForm
  , save
  , setContext
  , toForm
  , update
  , viewContext
  ) where

import Prelude

import Control.Comonad (extend, extract)
import Control.Comonad.Store (Store, experiment, peek, peeks, pos, runStore, seek, seeks, store)
import Control.Monad.Gen (frequency, resize, sized)
import Data.Bifoldable (class Bifoldable, bifoldMap, bifoldlDefault, bifoldrDefault)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either, either, hush, note)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Form.Required (Required(..))
import Data.Form.Result (Result(..), toEither)
import Data.Function (on)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Tuple.Nested ((/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen, runGen, stateful)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype Form ctx e a = Form (Store ctx (Result e a))

class FormContext ctx i o | ctx -> i o where
  current :: ctx -> i
  initial :: ctx -> i
  load :: i -> ctx -> ctx
  output :: ctx -> o
  update :: i -> ctx -> ctx

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance functorForm :: Functor (Form ctx e)

instance invariantForm :: Invariant (Form ctx e) where
  imap = imapF

instance bifunctorForm :: Bifunctor (Form ctx) where
  bimap f g = overForm $ map $ bimap f g

instance foldableForm :: Foldable (Form ctx e) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = foldMap f <<< extractResult

instance bifoldableForm :: Bifoldable (Form ctx) where
  bifoldr f = bifoldrDefault f
  bifoldl f = bifoldlDefault f
  bifoldMap f g = bifoldMap f g <<< extractResult

instance eqForm :: (Eq ctx, Eq e, Eq a) => Eq (Form ctx e a) where
  eq = on eq $ uncurry evalStore <<< runForm
    where
    evalStore p s = Tuple (p s) s

class IsForm f ctx e a | f -> ctx where
  toForm :: f e a -> Form ctx e a
  fromForm :: Form ctx e a -> f e a

instance isFormForm :: IsForm (Form ctx) ctx e a where
  toForm = identity
  fromForm = identity
else instance isFormNewtypeForm ::
  ( Newtype (f e a) (Form ctx e a)
  ) =>
  IsForm f ctx e a where
  toForm = unwrap
  fromForm = wrap

instance formContextForm ::
  ( FormContext ctx i o
  ) =>
  FormContext (Form ctx e a) i (Result e a) where
  current = current <<< viewContext
  initial = initial <<< viewContext
  load i = overContext (load i)
  output = extractResult
  update i = overContext (update i)

arbitraryFormContext
  :: forall ctx i o
   . FormContext ctx i o
  => Arbitrary i
  => Gen ctx
  -> Gen ctx
arbitraryFormContext blank = sized go
  where
  go size =
    frequency $ 1.0 /\ blank :|
      (Tuple nodeFreq <$> [ alter load, alter update ])
    where
    nodeFreq = toNumber size / 2.0
    half n = n / 2
    alter f = f <$> arbitrary <*> resize half (arbitraryFormContext blank)

instance arbitraryForm ::
  ( FormContext ctx i o
  , Arbitrary ctx
  , Arbitrary i
  , Arbitrary e
  , Arbitrary a
  , Coarbitrary o
  ) =>
  Arbitrary (Form ctx e a) where
  arbitrary = mkForm <$> arbitrary <*> stateful \state -> pure \o ->
    fst $ runGen (coarbitrary o arbitrary) state

instance showForm :: (Show ctx, Show e, Show a) => Show (Form ctx e a) where
  show f =
    "(Form " <> show (viewContext f) <> " " <> show (extractResult f) <> ")"

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

mkForm
  :: forall ctx i o e a
   . FormContext ctx i o
  => ctx
  -> (o -> Result e a)
  -> Form ctx e a
mkForm ctx validate = Form $ store (validate <<< output) ctx

form
  :: forall ctx i o e a
   . FormContext ctx i o
  => Monoid ctx
  => (o -> Result e a)
  -> Form ctx e a
form = mkForm mempty

form' :: forall ctx i o e. Monoid ctx => FormContext ctx i o => Form ctx e o
form' = form Ok

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

imapContext
  :: forall ctx ctx' e a
   . (ctx' -> ctx)
  -> (ctx -> ctx')
  -> Form ctx e a
  -> Form ctx' e a
imapContext f g = overForm (uncurry store <<< uncurry go <<< runStore)
  where
  go p s = Tuple (lcmap f p) $ g s

extendResult
  :: forall ctx e f a b
   . (Form ctx e a -> Result f b)
  -> Form ctx e a
  -> Form ctx f b
extendResult f = overForm $ extend $ f <<< Form

mapEither
  :: forall ctx e a b. (a -> Either e b) -> Form ctx e a -> Form ctx e b
mapEither f = extendResult $ either Error Ok <<< f <=< extractResult

setContext :: forall ctx e a. ctx -> Form ctx e a -> Form ctx e a
setContext ctx = overForm $ seek ctx

overContext :: forall ctx e a. (ctx -> ctx) -> Form ctx e a -> Form ctx e a
overContext f = overForm $ seeks f

extractResult :: forall ctx e a. Form ctx e a -> Result e a
extractResult = extract <<< unForm

peekResult :: forall ctx e a. ctx -> Form ctx e a -> Result e a
peekResult ctx = peek ctx <<< unForm

peeksResult :: forall ctx e a. (ctx -> ctx) -> Form ctx e a -> Result e a
peeksResult f = peeks f <<< unForm

required :: forall ctx e a. Form ctx e (Maybe a) -> Form ctx (Required e) a
required = mapEither (note Missing) <<< lmap Invalid

save :: forall ctx i o. FormContext ctx i o => ctx -> ctx
save ctx = load (current ctx) ctx

-------------------------------------------------------------------------------
-- Eliminators
-------------------------------------------------------------------------------

runForm :: forall ctx e a. Form ctx e a -> Tuple (ctx -> Result e a) ctx
runForm = runStore <<< unForm

viewContext :: forall ctx e a. Form ctx e a -> ctx
viewContext = pos <<< unForm

collectResults
  :: forall f ctx e a
   . Functor f
  => Filterable f
  => (ctx -> f ctx)
  -> Form ctx e a
  -> f a
collectResults f =
  filterMap (hush <<< toEither) <<< experiment f <<< unForm

dirty :: forall ctx i o. Eq i => FormContext ctx i o => ctx -> Boolean
dirty ctx = initial ctx /= current ctx

-------------------------------------------------------------------------------
-- Private helpers
-------------------------------------------------------------------------------

unForm :: forall ctx e a. Form ctx e a -> Store ctx (Result e a)
unForm (Form s) = s

overForm
  :: forall ctx ctx' e f a b
   . (Store ctx (Result e a) -> Store ctx' (Result f b))
  -> Form ctx e a
  -> Form ctx' f b
overForm f = Form <<< f <<< unForm
