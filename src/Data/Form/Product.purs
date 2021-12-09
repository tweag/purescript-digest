module Data.Form.Product
  ( ProductForm(..)
  , ProductContext(..)
  , biimapContext
  , lload
  , rload
  , biload
  , lupdate
  , rupdate
  , biupdate
  , loverContext
  , roverContext
  , bioverContext
  , lsetContext
  , rsetContext
  , bisetContext
  , product
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.Form (class FormContext, class IsForm, Form, current, extractResult, fromForm, imapContext, initial, load, mkForm, output, overContext, toForm, update, viewContext)
import Data.Form.Result (Result, ignore)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data ProductContext c1 c2 = ProductContext c1 c2

newtype ProductForm c1 c2 e a = ProductForm (Form (ProductContext c1 c2) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

product
  :: forall f1 f2 c1 c2 i1 i2 o1 o2 e1 e2 a b e
   . FormContext c1 i1 o1
  => FormContext c2 i2 o2
  => IsForm f1 c1 e1 a
  => IsForm f2 c2 e2 b
  => f1 e1 a
  -> f2 e2 b
  -> ProductForm (f1 e1 a) (f2 e2 b) e (a /\ b)
product f1 f2 = ProductForm $ mkForm (ProductContext f1 f2) ignore

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericProductForm :: Generic (ProductForm c1 c2 e a) _

derive instance newtypeProductForm :: Newtype (ProductForm c1 c2 e a) _

derive instance eqProductForm ::
  ( Eq c1
  , Eq c2
  , Eq e
  , Eq a
  ) =>
  Eq (ProductForm c1 c2 e a)

derive instance functorProductForm :: Functor (ProductForm c1 c2 e)

derive newtype instance invariantProductForm :: Invariant (ProductForm c1 c2 e)

derive newtype instance bifunctorProductForm :: Bifunctor (ProductForm c1 c2)

derive newtype instance foldableProductForm :: Foldable (ProductForm c1 c2 e)

derive newtype instance bifoldableProductForm :: Bifoldable (ProductForm c1 c2)

instance arbitraryNewtypeProductContext ::
  ( Arbitrary c1
  , Arbitrary c2
  , Arbitrary i1
  , Arbitrary i2
  , Arbitrary e1
  , Arbitrary e2
  , Arbitrary a
  , Arbitrary b
  , Coarbitrary o1
  , Coarbitrary o2
  , FormContext c1 i1 o1
  , FormContext c2 i2 o2
  , IsForm (f1 c1) c1 e1 a
  , IsForm (f2 c2) c2 e2 b
  ) =>
  Arbitrary (ProductContext (f1 c1 e1 a) (f2 c2 e2 b)) where
  arbitrary = map (bimap fromForm fromForm)
    $ ProductContext <$> arbitrary <*> arbitrary

derive instance genericProductContext :: Generic (ProductContext c1 c2) _
derive instance eqProductContext :: (Eq c1, Eq c2) => Eq (ProductContext c1 c2)
derive instance ordProductContext ::
  ( Ord c1
  , Ord c2
  ) =>
  Ord (ProductContext c1 c2)

derive instance functorProductContext :: Functor (ProductContext c1)
instance bifunctorProductContext :: Bifunctor ProductContext where
  bimap f g (ProductContext c1 c2) = ProductContext (f c1) (g c2)

instance showProductContext :: (Show c1, Show c2) => Show (ProductContext c1 c2) where
  show = genericShow

instance formContextProduct ::
  ( FormContext c1 i1 o1
  , FormContext c2 i2 o2
  , IsForm f1 c1 e1 a
  , IsForm f2 c2 e2 b
  ) =>
  FormContext
    (ProductContext (f1 e1 a) (f2 e2 b))
    (i1 /\ i2)
    (Result (e1 \/ e2) (a /\ b)) where
  current (ProductContext f1 f2) = current (toForm f1) /\ current (toForm f2)
  initial (ProductContext f1 f2) = initial (toForm f1) /\ initial (toForm f2)
  load (i1 /\ i2) (ProductContext f1 f2) =
    ProductContext (fromForm $ load i1 $ toForm f1)
      (fromForm $ load i2 $ toForm f2)
  output (ProductContext f1 f2) =
    Tuple <$> lmap Left (output (toForm f1)) <*> lmap Right (output (toForm f2))
  update (i1 /\ i2) (ProductContext f1 f2) =
    ProductContext (fromForm $ update i1 $ toForm f1)
      (fromForm $ update i2 $ toForm f2)

instance formContextProductForm ::
  ( FormContext (ProductContext c1 c2) i o
  ) =>
  FormContext (ProductForm c1 c2 e a) i (Result e a) where
  current = current <<< viewContext <<< unwrap
  initial = initial <<< viewContext <<< unwrap
  load i = over ProductForm $ overContext (load i)
  output = extractResult <<< unwrap
  update i = over ProductForm $ overContext (update i)

lsetContext
  :: forall c1 c2 e a. c1 -> ProductForm c1 c2 e a -> ProductForm c1 c2 e a
lsetContext = loverContext <<< const

rsetContext
  :: forall c1 c2 e a. c2 -> ProductForm c1 c2 e a -> ProductForm c1 c2 e a
rsetContext = roverContext <<< const

bisetContext
  :: forall c1 c2 e a
   . c1
  -> c2
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
bisetContext c1 = bioverContext (const c1) <<< const

loverContext
  :: forall c1 c2 e a
   . (c1 -> c1)
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
loverContext = over ProductForm <<< overContext <<< lmap

roverContext
  :: forall c1 c2 e a
   . (c2 -> c2)
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
roverContext = over ProductForm <<< overContext <<< map

bioverContext
  :: forall c1 c2 e a
   . (c1 -> c1)
  -> (c2 -> c2)
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
bioverContext f1 = over ProductForm <<< overContext <<< bimap f1

lload
  :: forall c1 c2 i1 i2 o e a
   . FormContext (ProductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
lload i f = load (lmap (const i) $ current f) f

rload
  :: forall c1 c2 i1 i2 o e a
   . FormContext (ProductForm c1 c2 e a) (i1 /\ i2) o
  => i2
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
rload i f = load (map (const i) $ current f) f

biload
  :: forall c1 c2 i1 i2 o e a
   . FormContext (ProductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> i2
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
biload i1 i2 = load (i1 /\ i2)

lupdate
  :: forall c1 c2 i1 i2 o e a
   . FormContext (ProductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
lupdate i f = update (lmap (const i) $ current f) f

rupdate
  :: forall c1 c2 i1 i2 o e a
   . FormContext (ProductForm c1 c2 e a) (i1 /\ i2) o
  => i2
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
rupdate i f = update (map (const i) $ current f) f

biupdate
  :: forall c1 c2 i1 i2 o e a
   . FormContext (ProductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> i2
  -> ProductForm c1 c2 e a
  -> ProductForm c1 c2 e a
biupdate i1 i2 = update (i1 /\ i2)

biimapContext
  :: forall c1 c1' c2 c2' e a
   . (c1' -> c1)
  -> (c1 -> c1')
  -> (c2' -> c2)
  -> (c2 -> c2')
  -> ProductForm c1 c2 e a
  -> ProductForm c1' c2' e a
biimapContext f g i j = over ProductForm $ imapContext (bimap f i) (bimap g j)
