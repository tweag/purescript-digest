module Data.Form.Required where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Plus (class Plus)
import Data.Compactable (class Compactable, compact)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Eq (class Eq1)
import Data.Filterable (class Filterable, filterDefault, partitionDefault)
import Data.Foldable (class Foldable)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.Witherable (class Witherable)

-- | The `Required` type is used to make the error for an optional form into
-- | an error for a reqired form.
data Required a = Missing | Invalid a

instance functorRequired :: Functor Required where
  map fn (Invalid x) = Invalid (fn x)
  map _ _ = Missing

instance compactableRequired :: Compactable Required where
  compact Missing = Missing
  compact (Invalid Nothing) = Missing
  compact (Invalid (Just a)) = Invalid a
  separate Missing = { left: Missing, right: Missing }
  separate (Invalid e) = case e of
    Left l -> { left: Invalid l, right: Missing }
    Right r -> { left: Missing, right: Invalid r }

instance filterableRequired :: Filterable Required where
  partitionMap _ Missing = { left: Missing, right: Missing }
  partitionMap p (Invalid x) = case p x of
    Left a -> { left: Invalid a, right: Missing }
    Right b -> { left: Missing, right: Invalid b }
  partition p = partitionDefault p
  filterMap p = compact <<< map p
  filter p = filterDefault p

instance witherableRequired :: Witherable Required where
  wilt _ Missing = pure { left: Missing, right: Missing }
  wilt p (Invalid x) = map convert (p x)
    where
    convert (Left l) = { left: Invalid l, right: Missing }
    convert (Right r) = { left: Missing, right: Invalid r }

  wither _ Missing = pure Missing
  wither p (Invalid x) = map convert (p x)
    where
    convert Nothing = Missing
    convert (Just a) = Invalid a

instance foldableRequired :: Foldable Required where
  foldr _ z Missing = z
  foldr f z (Invalid x) = x `f` z
  foldl _ z Missing = z
  foldl f z (Invalid x) = z `f` x
  foldMap _ Missing = mempty
  foldMap f (Invalid x) = f x

instance traversableRequired :: Traversable Required where
  traverse _ Missing = pure Missing
  traverse f (Invalid x) = Invalid <$> f x
  sequence Missing = pure Missing
  sequence (Invalid x) = Invalid <$> x

instance applyRequired :: Apply Required where
  apply (Invalid fn) x = fn <$> x
  apply Missing _ = Missing

instance applicativeRequired :: Applicative Required where
  pure = Invalid

instance altRequired :: Alt Required where
  alt Missing r = r
  alt l _ = l

instance plusRequired :: Plus Required where
  empty = Missing

instance alternativeRequired :: Alternative Required

instance bindRequired :: Bind Required where
  bind (Invalid x) k = k x
  bind Missing _ = Missing

instance monadRequired :: Monad Required

instance extendRequired :: Extend Required where
  extend _ Missing = Missing
  extend f x = Invalid (f x)

instance invariantRequired :: Invariant Required where
  imap = imapF

instance semigroupRequired :: Semigroup a => Semigroup (Required a) where
  append Missing y = y
  append x Missing = x
  append (Invalid x) (Invalid y) = Invalid (x <> y)

instance monoidRequired :: Semigroup a => Monoid (Required a) where
  mempty = Missing

derive instance eqRequired :: Eq a => Eq (Required a)

instance eq1Required :: Eq1 Required where
  eq1 = eq

derive instance ordRequired :: Ord a => Ord (Required a)

instance ord1Required :: Ord1 Required where
  compare1 = compare

instance boundedRequired :: Bounded a => Bounded (Required a) where
  top = Invalid top
  bottom = Missing

instance enumRequired :: (Bounded a, Enum a) => Enum (Required a) where
  succ = genericSucc
  pred = genericPred

instance boundedEnumRequired :: BoundedEnum a => BoundedEnum (Required a) where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance showRequired :: Show a => Show (Required a) where
  show (Invalid x) = "(Invalid " <> show x <> ")"
  show Missing = "Missing"

derive instance genericRequired :: Generic (Required a) _

-- | Takes a default value, a function, and a `Required` value. If the
-- | `Required`
-- | value is `Missing` the default value is returned, otherwise the function
-- | is applied to the value inside the `Invalid` and the result is returned.
-- |
-- | ``` purescript
-- | required x f Missing == x
-- | required x f (Invalid y) == f y
-- | ```
required :: forall a b. b -> (a -> b) -> Required a -> b
required b _ Missing = b
required _ f (Invalid a) = f a

-- | Similar to `required` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard `required` has
-- | to evaluate the default value before returning the result, whereas here
-- | the value is only computed when the `Required` is known to be `Missing`.
-- |
-- | ``` purescript
-- | required' (\_ -> x) f Missing == x
-- | required' (\_ -> x) f (Invalid y) == f y
-- | ```
required' :: forall a b. (Unit -> b) -> (a -> b) -> Required a -> b
required' g _ Missing = g unit
required' _ f (Invalid a) = f a

-- | Takes a default value, and a `Required` value. If the `Required` value is
-- | `Missing` the default value is returned, otherwise the value inside the
-- | `Invalid` is returned.
-- |
-- | ``` purescript
-- | fromRequired x Missing == x
-- | fromRequired x (Invalid y) == y
-- | ```
fromRequired :: forall a. a -> Required a -> a
fromRequired a = required a identity

-- | Similar to `fromRequired` but for use in cases where the default value may be
-- | expensive to compute. As PureScript is not lazy, the standard
-- | `fromRequired`
-- | has to evaluate the default value before returning the result, whereas here
-- | the value is only computed when the `Required` is known to be `Missing`.
-- |
-- | ``` purescript
-- | fromRequired' (\_ -> x) Missing == x
-- | fromRequired' (\_ -> x) (Invalid y) == y
-- | ```
fromRequired' :: forall a. (Unit -> a) -> Required a -> a
fromRequired' a = required' a identity

-- | Returns `true` when the `Required` value was constructed with `Invalid`.
isInvalid :: forall a. Required a -> Boolean
isInvalid = required false (const true)

-- | Returns `true` when the `Required` value is `Missing`.
isMissing :: forall a. Required a -> Boolean
isMissing = required true (const false)
