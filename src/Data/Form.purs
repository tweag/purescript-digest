module Data.Form
  ( Form
  , arbitraryFormContext
  , class ArbitraryFormContext
  , class FormContext
  , class IsForm
  , coarbitraryFormContext
  , collectResults
  , ctx_current
  , ctx_initial
  , ctx_load
  , ctx_output
  , ctx_update
  , current
  , dirty
  , extendContext
  , extendResult
  , extractContext
  , extractResult
  , form
  , form'
  , fromForm
  , genBlank
  , imapContext
  , initial
  , load
  , loadFrom
  , mapContext
  , mapEither
  , mapResult
  , peekResult
  , peeksResult
  , putContext
  , required
  , runForm
  , save
  , toForm
  , update
  , updateTo
  ) where

import Prelude

import Control.Comonad (extend, extract)
import Control.Comonad.Store
  ( Store
  , experiment
  , peek
  , peeks
  , pos
  , runStore
  , seek
  , store
  )
import Control.Monad.Gen (frequency, resize, sized)
import Data.Bifoldable
  ( class Bifoldable
  , bifoldMap
  , bifoldlDefault
  , bifoldrDefault
  )
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either, either, hush, note)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Form.Result (Result(..), toEither)
import Data.Function (on)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Tuple.Nested ((/\))
import Test.QuickCheck
  ( class Arbitrary
  , class Coarbitrary
  , arbitrary
  , coarbitrary
  )
import Test.QuickCheck.Gen (Gen, runGen, stateful)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype Form ctx e a = Form (Store ctx (Result e a))

class FormContext ctx i o | ctx -> i o where
  ctx_current :: ctx -> i
  ctx_initial :: ctx -> i
  ctx_load :: i -> ctx -> ctx
  ctx_output :: ctx -> o
  ctx_update :: i -> ctx -> ctx

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance functorForm :: Functor (Form ctx e)

instance invariantForm :: Invariant (Form ctx e) where
  imap = imapF

instance bifunctorForm :: Bifunctor (Form ctx) where
  bimap f g = overStore $ map $ bimap f g

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

overForm
  :: forall f f' c c' e e' a a'
   . IsForm f c e a
  => IsForm f' c' e' a'
  => (Form c e a -> Form c' e' a')
  -> f e a
  -> f' e' a'
overForm f = fromForm <<< f <<< toForm

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
  ctx_current = current
  ctx_initial = initial
  ctx_load = load
  ctx_output = extractResult
  ctx_update = update

instance arbitraryFormContextForm ::
  ( ArbitraryFormContext ctx i o
  , Arbitrary i
  , Arbitrary e
  , Arbitrary a
  , Coarbitrary o
  ) =>
  ArbitraryFormContext (Form ctx e a) i (Result e a) where
  genBlank = genForm genBlank

class FormContext ctx i o <= ArbitraryFormContext ctx i o | ctx -> i o where
  genBlank :: Gen ctx

coarbitraryFormContext
  :: forall ctx i o r
   . FormContext ctx i o
  => Coarbitrary o
  => ctx
  -> Gen r
  -> Gen r
coarbitraryFormContext = coarbitrary <<< ctx_output

arbitraryFormContext
  :: forall ctx i o
   . ArbitraryFormContext ctx i o
  => Arbitrary i
  => Gen ctx
arbitraryFormContext = sized go
  where
  go size =
    frequency $ 1.0 /\ genBlank :|
      (Tuple nodeFreq <$> [ alter ctx_load, alter ctx_update ])
    where
    nodeFreq = toNumber size / 2.0
    half n = n / 2
    alter f = f <$> arbitrary <*> resize half arbitraryFormContext

genForm
  :: forall ctx i o e a
   . FormContext ctx i o
  => Arbitrary i
  => Arbitrary e
  => Arbitrary a
  => Coarbitrary o
  => Gen ctx
  -> Gen (Form ctx e a)
genForm genCtx =
  form <$> genCtx <*> stateful \state -> pure \o ->
    fst $ runGen (coarbitrary o arbitrary) state

instance arbitraryForm ::
  ( FormContext ctx i o
  , Arbitrary ctx
  , Arbitrary i
  , Arbitrary e
  , Arbitrary a
  , Coarbitrary o
  ) =>
  Arbitrary (Form ctx e a) where
  arbitrary = genForm arbitrary

instance coarbitraryForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (Form ctx e a) where
  coarbitrary = coarbitrary <<< extractResult

instance showForm :: (Show ctx, Show e, Show a) => Show (Form ctx e a) where
  show f =
    "(Form " <> show (extractContext f) <> " " <> show (extractResult f) <> ")"

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

form
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => ctx
  -> (o -> Result e a)
  -> f e a
form ctx validate = fromForm $ Form $ store (validate <<< ctx_output) ctx

form'
  :: forall ctx f i o e
   . FormContext ctx i o
  => IsForm f ctx e o
  => ctx
  -> f e o
form' ctx = form ctx Ok

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

putContext :: forall f ctx e a. IsForm f ctx e a => ctx -> f e a -> f e a
putContext ctx = overStore $ seek ctx

imapContext
  :: forall f f' ctx ctx' e a
   . IsForm f ctx e a
  => IsForm f' ctx' e a
  => (ctx' -> ctx)
  -> (ctx -> ctx')
  -> f e a
  -> f' e a
imapContext f g = overStore $ uncurry store <<< uncurry go <<< runStore
  where
  go p s = Tuple (lcmap f p) $ g s

extendContext
  :: forall ctx f e a. IsForm f ctx e a => (f e a -> ctx) -> f e a -> f e a
extendContext f = uncurry putContext <<< (f &&& identity)

mapContext
  :: forall f ctx e a. IsForm f ctx e a => (ctx -> ctx) -> f e a -> f e a
mapContext f = extendContext $ f <<< extractContext

extendResult
  :: forall f ctx e e' a b
   . IsForm f ctx e a
  => IsForm f ctx e' b
  => (Form ctx e a -> Result e' b)
  -> f e a
  -> f e' b
extendResult f = overStore $ extend $ f <<< Form

mapResult
  :: forall f ctx e a b
   . IsForm f ctx e a
  => IsForm f ctx e b
  => (a -> Result e b)
  -> f e a
  -> f e b
mapResult f = extendResult $ f <=< extractResult

mapEither
  :: forall f ctx e a b
   . IsForm f ctx e a
  => IsForm f ctx e b
  => (a -> Either e b)
  -> f e a
  -> f e b
mapEither = mapResult <<< compose (either Error Ok)

peekResult :: forall f ctx e a. IsForm f ctx e a => ctx -> f e a -> Result e a
peekResult ctx = peek ctx <<< unForm

peeksResult
  :: forall f ctx e a. IsForm f ctx e a => (ctx -> ctx) -> f e a -> Result e a
peeksResult f = peeks f <<< unForm

required
  :: forall f ctx e a
   . Bifunctor f
  => IsForm f ctx e (Maybe a)
  => IsForm f ctx (Maybe e) (Maybe a)
  => IsForm f ctx (Maybe e) a
  => f e (Maybe a)
  -> f (Maybe e) a
required = mapEither (note Nothing) <<< lmap Just

load
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => i
  -> f e a
  -> f e a
load = mapContext <<< ctx_load

loadFrom
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => (i -> i)
  -> f e a
  -> f e a
loadFrom f = uncurry load <<< (f <<< initial &&& identity)

update
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => i
  -> f e a
  -> f e a
update = mapContext <<< ctx_update

updateTo
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => (i -> i)
  -> f e a
  -> f e a
updateTo f = uncurry update <<< (f <<< current &&& identity)

save
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => f e a
  -> f e a
save = uncurry load <<< (current &&& identity)

-------------------------------------------------------------------------------
-- Eliminators
-------------------------------------------------------------------------------

runForm
  :: forall f ctx e a
   . IsForm f ctx e a
  => f e a
  -> Tuple (ctx -> Result e a) ctx
runForm = runStore <<< unForm

extractContext :: forall ctx f e a. IsForm f ctx e a => f e a -> ctx
extractContext = pos <<< unForm

current
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => f e a
  -> i
current = ctx_current <<< extractContext

initial
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => f e a
  -> i
initial = ctx_initial <<< extractContext

extractResult :: forall f ctx e a. IsForm f ctx e a => f e a -> Result e a
extractResult = extract <<< unForm

collectResults
  :: forall form f ctx e a
   . Functor f
  => Filterable f
  => IsForm form ctx e a
  => (ctx -> f ctx)
  -> form e a
  -> f a
collectResults f =
  filterMap (hush <<< toEither) <<< experiment f <<< unForm

dirty
  :: forall ctx f i o e a
   . FormContext ctx i o
  => IsForm f ctx e a
  => FormContext ctx i o
  => Eq i
  => f e a
  -> Boolean
dirty = uncurry notEq <<< (initial &&& current)

-------------------------------------------------------------------------------
-- Private helpers
-------------------------------------------------------------------------------

unForm :: forall f ctx e a. IsForm f ctx e a => f e a -> Store ctx (Result e a)
unForm f = case toForm f of
  Form s -> s

overStore
  :: forall f f' ctx ctx' e e' a b
   . IsForm f ctx e a
  => IsForm f' ctx' e' b
  => (Store ctx (Result e a) -> Store ctx' (Result e' b))
  -> f e a
  -> f' e' b
overStore f = overForm $ Form <<< f <<< unForm
