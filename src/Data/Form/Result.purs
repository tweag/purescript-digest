module Data.Form.Result where

import Prelude

import Control.Alt (class Alt)
import Control.Extend (class Extend)
import Control.Monad.Gen (frequency)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Either (Either(..), either)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty ((:|))
import Data.Ord (class Ord1)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.QuickCheck.Gen (perturbGen)

-- | The `Result` type is used to represent the result of a form validation. It
-- | is isomorphic to `Result (Maybe e) a` where `Left Nothing` corresponds to
-- | `Unevaluated`.
-- |
-- | The motivation for equiping `Result` with this case is to allow sub-form
-- | validation steps to fail without having to propagate their failure
-- | information all the way up the chain - as in practice, the aggregate
-- | context is queried for sub-forms, which store their error information
-- | already. This is opposed to querying some aggregate error structure for
-- | sub-errors.
data Result e a = Unevaluated | Error e | Ok a

derive instance functorResult :: Functor (Result e)

instance bifunctorResult :: Bifunctor Result where
  bimap _ _ Unevaluated = Unevaluated
  bimap f _ (Error e) = Error $ f e
  bimap _ g (Ok a) = Ok $ g a

instance foldableResult :: Foldable (Result e) where
  foldr f z (Ok x) = f x z
  foldr _ z _ = z
  foldl f z (Ok x) = f z x
  foldl _ z _ = z
  foldMap f (Ok x) = f x
  foldMap _ _ = mempty

instance bifoldableResult :: Bifoldable Result where
  bifoldr f _ z (Error x) = f x z
  bifoldr _ g z (Ok x) = g x z
  bifoldr _ _ z _ = z
  bifoldl f _ z (Error x) = f z x
  bifoldl _ g z (Ok x) = g z x
  bifoldl _ _ z _ = z
  bifoldMap f _ (Error x) = f x
  bifoldMap _ g (Ok x) = g x
  bifoldMap _ _ _ = mempty

instance traversableResult :: Traversable (Result e) where
  traverse _ Unevaluated = pure Unevaluated
  traverse _ (Error x) = pure (Error x)
  traverse f (Ok x) = Ok <$> f x
  sequence Unevaluated = pure Unevaluated
  sequence (Error x) = pure (Error x)
  sequence (Ok x) = Ok <$> x

instance bitraversableResult :: Bitraversable Result where
  bitraverse _ _ Unevaluated = pure Unevaluated
  bitraverse f _ (Error a) = Error <$> f a
  bitraverse _ g (Ok b) = Ok <$> g b
  bisequence Unevaluated = pure Unevaluated
  bisequence (Error a) = Error <$> a
  bisequence (Ok b) = Ok <$> b

derive instance genericResult :: Generic (Result e a) _

instance invariantResult :: Invariant (Result e) where
  imap = imapF

instance applyResult :: Apply (Result e) where
  apply Unevaluated _ = Unevaluated
  apply (Error e) _ = Error e
  apply (Ok f) r = f <$> r

instance applicativeResult :: Applicative (Result e) where
  pure = Ok

instance altResult :: Alt (Result e) where
  alt (Error _) r = r
  alt l _ = l

instance bindResult :: Bind (Result e) where
  bind = result (const Unevaluated) (\e _ -> Error e) (\a f -> f a)

instance monadResult :: Monad (Result e)

instance extendResult :: Extend (Result e) where
  extend _ (Error y) = Error y
  extend f x = Ok (f x)

instance showResult :: (Show e, Show a) => Show (Result e a) where
  show = genericShow

derive instance eqResult :: (Eq e, Eq a) => Eq (Result e a)

derive instance eq1Result :: Eq e => Eq1 (Result e)

derive instance ordResult :: (Ord e, Ord a) => Ord (Result e a)

derive instance ord1Result :: Ord e => Ord1 (Result e)

instance boundedResult :: (Bounded e, Bounded a) => Bounded (Result e a) where
  top = Ok top
  bottom = Unevaluated

instance semigroupResult :: (Semigroup a) => Semigroup (Result e a) where
  append x y = append <$> x <*> y

instance coarbitraryResult ::
  ( Coarbitrary e
  , Coarbitrary a
  ) =>
  Coarbitrary (Result e a) where
  coarbitrary Unevaluated = perturbGen 1.0
  coarbitrary (Error e) = perturbGen 2.0 <<< coarbitrary e
  coarbitrary (Ok a) = perturbGen 3.0 <<< coarbitrary a

instance arbitraryResult ::
  ( Arbitrary e
  , Arbitrary a
  ) =>
  Arbitrary (Result e a) where
  arbitrary =
    frequency
      $ Tuple 0.1 (pure Unevaluated)
          :|
            [ Tuple 0.4 (Error <$> arbitrary)
            , Tuple 0.5 (Ok <$> arbitrary)
            ]

result :: forall e a b. b -> (e -> b) -> (a -> b) -> Result e a -> b
result b _ _ Unevaluated = b
result _ f _ (Error e) = f e
result _ _ g (Ok a) = g a

-- | Returns `true` when the `Result` value was constructed with `Unevaluated`.
isUnevaluated :: forall e a. Result e a -> Boolean
isUnevaluated = result true (const false) (const false)

-- | Returns `true` when the `Result` value was constructed with `Error`.
isError :: forall e a. Result e a -> Boolean
isError = result false (const true) (const false)

-- | Returns `true` when the `Result` value was constructed with `Ok`.
isOk :: forall e a. Result e a -> Boolean
isOk = result false (const false) (const true)

-- | A function that extracts the value from the `Error` data constructor.
-- | The first argument is a default value, which will be returned in the
-- | case where an `Ok` or `Unevaluated` is passed to `fromError`.
fromError :: forall e a. e -> Result e a -> e
fromError _ (Error a) = a
fromError default _ = default

-- | Similar to `fromError` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `fromError`
-- | has to evaluate the default value before returning the result,
-- | whereas here the value is only computed when the `Result` is known
-- | to be `Ok` or `Unevaluated`.
fromError' :: forall e a. (Unit -> e) -> Result e a -> e
fromError' _ (Error a) = a
fromError' default _ = default unit

-- | A function that extracts the value from the `Ok` data constructor.
-- | The first argument is a default value, which will be returned in the
-- | case where an `Error` or `Unevaluated` is passed to `fromOk`.
fromOk :: forall e a. a -> Result e a -> a
fromOk _ (Ok b) = b
fromOk default _ = default

-- | Similar to `fromOk` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `fromOk`
-- | has to evaluate the default value before returning the result,
-- | whereas here the value is only computed when the `Result` is known
-- | to be `Error` or `Unevaluated`.
fromOk' :: forall e a. (Unit -> a) -> Result e a -> a
fromOk' _ (Ok b) = b
fromOk' default _ = default unit

-- | Turns an `Either` into a `Result`.
fromEither :: forall e. Either (Maybe e) ~> Result e
fromEither = either (maybe Unevaluated Error) Ok

-- | Turns a `Result` into an `Either`.
toEither :: forall e. Result e ~> Either (Maybe e)
toEither = result (Left Nothing) (Left <<< Just) Right

-- | Ignore any errors in the first result by converting them to an
-- | `Unevaluated`.
ignore :: forall e e' a. Result e a -> Result e' a
ignore (Ok a) = Ok a
ignore _ = Unevaluated
