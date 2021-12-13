module Data.Form.Traversable
  ( TraversableForm(..)
  , TraversableContext
  , getIndexForm
  , setIndexForm
  , traversable
  , traversableValidate
  ) where

import Prelude

import Control.Alternative (class Plus)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Form
  ( class FormContext
  , class IsForm
  , Form(..)
  , arbitraryForm
  , clear
  , currentContext
  , formValidate
  , initial
  , initialContext
  , load
  , loadContext
  , loadsContext
  , result
  , save
  , updateContext
  , updatesContext
  )
import Data.Form.Result (Result(..), ignore)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Lens (over, preview, (^?))
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.Index (class Index, ix)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, head, singleton, tail, (:|))
import Data.Profunctor (lcmap)
import Data.Traversable (class Traversable, traverse)
import Test.QuickCheck (class Arbitrary, class Coarbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

newtype TraversableContext t f = TC (NonEmpty t f)

unTC :: forall t f. TraversableContext t f -> NonEmpty t f
unTC (TC t) = t

overTC
  :: forall t f u g
   . (NonEmpty t f -> NonEmpty u g)
  -> TraversableContext t f
  -> TraversableContext u g
overTC f (TC t) = TC $ f t

newtype TraversableForm t f e a =
  TraversableForm (Form (TraversableContext t f) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

traversableValidate
  :: forall t f i e' e a c
   . Plus t
  => Traversable t
  => IsForm f i
  => f e' a
  -> (t a -> Result e c)
  -> TraversableForm t (f e' a) e c
traversableValidate template validate =
  formValidate
    (TC $ singleton template)
    (validate <=< validateTraversable <<< tail <<< unTC)
  where
  validateTraversable = ignore <<< traverse result

traversable
  :: forall t f i e' e a
   . Plus t
  => Traversable t
  => IsForm f i
  => f e' a
  -> TraversableForm t (f e' a) e (t a)
traversable f = traversableValidate f Ok

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericTraversableForm :: Generic (TraversableForm c1 c2 e a) _

derive instance newtypeTraversableForm :: Newtype (TraversableForm c1 c2 e a) _

derive instance eqTraversableContext ::
  ( Eq1 t
  , Eq f
  ) =>
  Eq (TraversableContext t f)

derive instance eqTraversableForm ::
  ( Eq1 t
  , Eq f
  , Eq e
  , Eq a
  ) =>
  Eq (TraversableForm t f e a)

derive instance functorTraversableForm :: Functor (TraversableForm c1 c2 e)

derive newtype instance invariantTraversableForm ::
  Invariant (TraversableForm c1 c2 e)

derive newtype instance bifunctorTraversableForm ::
  Bifunctor (TraversableForm c1 c2)

derive newtype instance foldableTraversableForm ::
  Foldable (TraversableForm c1 c2 e)

derive newtype instance bifoldableTraversableForm ::
  Bifoldable (TraversableForm c1 c2)

derive newtype instance showTraversableContext ::
  ( Show (t f)
  , Show f
  ) =>
  Show (TraversableContext t f)

derive newtype instance showCoproductForm ::
  ( Show f
  , Show (t f)
  , Show e
  , Show a
  ) =>
  Show (TraversableForm t f e a)

instance formContextTraversableContext ::
  ( IsForm f ctx
  , FormContext ctx i
  , Functor t
  , Plus t
  ) =>
  FormContext (TraversableContext t (f e a)) (t i) where
  clearInput = overTC $ singleton <<< save <<< clear <<< head
  getInput = map initial <<< tail <<< unTC
  setInput is = overTC \(template :| _) ->
    template :| (flip load template <$> is)

derive newtype instance arbitraryTraversableContext ::
  ( Arbitrary (t f)
  , Arbitrary f
  ) =>
  Arbitrary (TraversableContext t f)

instance arbitraryTraversableForm ::
  ( IsForm f ctx
  , FormContext ctx i
  , Arbitrary u
  , Arbitrary e
  , Arbitrary (f e' a)
  , Arbitrary (t (f e' a))
  , Arbitrary (t i)
  , Coarbitrary (f e' a)
  , Coarbitrary (t (f e' a))
  , Coarbitrary (t a)
  , Plus t
  , Traversable t
  ) =>
  Arbitrary (TraversableForm t (f e' a) e u) where
  arbitrary = arbitraryForm $ lcmap (head <<< unTC) traversableValidate

derive newtype instance coarbitraryTraversableForm ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (TraversableForm t ctx e a)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

getIndexForm
  :: forall index t f ctx e' a' e a
   . IsForm f ctx
  => Index (t (f e' a')) index (f e' a')
  => index
  -> TraversableForm t (f e' a') e a
  -> Maybe (f e' a')
getIndexForm index tf = do
  currentForm <- preview (ix index) (tail $ unTC $ currentContext tf)
  let current = currentContext currentForm
  let template :| forms = unTC $ initialContext tf
  pure $ updateContext current $ fromMaybe template $ forms ^? ix index

setIndexForm
  :: forall index t f ctx e' a' e a
   . IsForm f ctx
  => Index (t (f e' a')) index (f e' a')
  => index
  -> f e' a'
  -> TraversableForm t (f e' a') e a
  -> TraversableForm t (f e' a') e a
setIndexForm index f =
  updatesContext (go currentContext) <<< loadsContext (go initialContext)
  where
  go getter = overTC \(template :| forms) ->
    template :| over (ix index) (loadContext $ getter f) forms

instance indexTraversableForm ::
  ( IsForm f ctx
  , Index (t (f e' a')) index (f e' a')
  ) =>
  Index (TraversableForm t (f e' a') e a) index (f e' a') where
  ix index = affineTraversal set pre
    where
    set = flip $ setIndexForm index
    pre t = maybe (Left t) Right $ getIndexForm index t
