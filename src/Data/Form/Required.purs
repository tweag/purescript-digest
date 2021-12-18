module Data.Form.Required where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Plus (class Plus)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic
  ( genericCardinality
  , genericFromEnum
  , genericPred
  , genericSucc
  , genericToEnum
  )
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)

-- | The `Required` type is used to make the error for an optional form into
-- | an error for a reqired form.
data Required a = Absent | Present a

type Required' = forall e. Required e

instance functorRequired :: Functor Required where
  map fn (Present x) = Present (fn x)
  map _ _ = Absent

instance foldableRequired :: Foldable Required where
  foldr _ z Absent = z
  foldr f z (Present x) = x `f` z
  foldl _ z Absent = z
  foldl f z (Present x) = z `f` x
  foldMap _ Absent = mempty
  foldMap f (Present x) = f x

instance traversableRequired :: Traversable Required where
  traverse _ Absent = pure Absent
  traverse f (Present x) = Present <$> f x
  sequence Absent = pure Absent
  sequence (Present x) = Present <$> x

instance applyRequired :: Apply Required where
  apply (Present fn) x = fn <$> x
  apply Absent _ = Absent

instance applicativeRequired :: Applicative Required where
  pure = Present

instance altRequired :: Alt Required where
  alt Absent r = r
  alt l _ = l

instance plusRequired :: Plus Required where
  empty = Absent

instance alternativeRequired :: Alternative Required

instance bindRequired :: Bind Required where
  bind (Present x) k = k x
  bind Absent _ = Absent

instance monadRequired :: Monad Required

instance extendRequired :: Extend Required where
  extend _ Absent = Absent
  extend f x = Present (f x)

instance invariantRequired :: Invariant Required where
  imap = imapF

instance semigroupRequired :: Semigroup a => Semigroup (Required a) where
  append Absent y = y
  append x Absent = x
  append (Present x) (Present y) = Present (x <> y)

instance monoidRequired :: Semigroup a => Monoid (Required a) where
  mempty = Absent

derive instance eqRequired :: Eq a => Eq (Required a)

instance eq1Required :: Eq1 Required where
  eq1 = eq

derive instance ordRequired :: Ord a => Ord (Required a)

instance ord1Required :: Ord1 Required where
  compare1 = compare

instance boundedRequired :: Bounded a => Bounded (Required a) where
  top = Present top
  bottom = Absent

instance enumRequired :: (Bounded a, Enum a) => Enum (Required a) where
  succ = genericSucc
  pred = genericPred

instance boundedEnumRequired :: BoundedEnum a => BoundedEnum (Required a) where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showRequired :: Show a => Show (Required a) where
  show (Present x) = "(Present " <> show x <> ")"
  show Absent = "Absent"

derive instance genericRequired :: Generic (Required a) _

-- | Takes a default value, a function, and a `Required` value. If the
-- | `Required`
-- | value is `Absent` the default value is returned, otherwise the function
-- | is applied to the value inside the `Present` and the result is returned.
-- |
-- | ``` purescript
-- | required x f Absent == x
-- | required x f (Present y) == f y
-- | ```
required :: forall a b. b -> (a -> b) -> Required a -> b
required b _ Absent = b
required _ f (Present a) = f a

-- | Similar to `required` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `required` has
-- | to evaluate the default value before returning the result, whereas here
-- | the value is only computed when the `Required` is known to be `Absent`.
-- |
-- | ``` purescript
-- | required' (\_ -> x) f Absent == x
-- | required' (\_ -> x) f (Present y) == f y
-- | ```
required' :: forall a b. (Unit -> b) -> (a -> b) -> Required a -> b
required' g _ Absent = g unit
required' _ f (Present a) = f a

-- | Takes a default value, and a `Required` value. If the `Required` value is
-- | `Absent` the default value is returned, otherwise the value inside the
-- | `Present` is returned.
-- |
-- | ``` purescript
-- | fromRequired x Absent == x
-- | fromRequired x (Present y) == y
-- | ```
fromRequired :: forall a. a -> Required a -> a
fromRequired a = required a identity

-- | Similar to `fromRequired` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard
-- | `fromRequired`
-- | has to evaluate the default value before returning the result, whereas here
-- | the value is only computed when the `Required` is known to be `Absent`.
-- |
-- | ``` purescript
-- | fromRequired' (\_ -> x) Absent == x
-- | fromRequired' (\_ -> x) (Present y) == y
-- | ```
fromRequired' :: forall a. (Unit -> a) -> Required a -> a
fromRequired' a = required' a identity

-- | Returns `true` when the `Required` value was constructed with `Present`.
isAbsent :: forall a. Required a -> Boolean
isAbsent = required false (const true)

-- | Returns `true` when the `Required` value is `Absent`.
isPresent :: forall a. Required a -> Boolean
isPresent = required true (const false)
