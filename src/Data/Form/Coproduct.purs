module Data.Form.Coproduct
  ( CoproductForm(..)
  , CoproductContext(..)
  , choose
  , loadChoice
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
  , coproduct
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
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data CoproductContext c1 c2 = CoproductContext Boolean Boolean c1 c2

newtype CoproductForm c1 c2 e a = CoproductForm
  (Form (CoproductContext c1 c2) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

coproduct
  :: forall f1 f2 c1 c2 i1 i2 o1 o2 e1 e2 a b e
   . FormContext c1 i1 o1
  => FormContext c2 i2 o2
  => IsForm f1 c1 e1 a
  => IsForm f2 c2 e2 b
  => Boolean
  -> f1 e1 a
  -> f2 e2 b
  -> CoproductForm (f1 e1 a) (f2 e2 b) e (a \/ b)
coproduct choice f1 f2 =
  CoproductForm $ mkForm (CoproductContext choice choice f1 f2) validate
  where
  validate (Left a) = Left <$> ignore a
  validate (Right b) = Right <$> ignore b

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericCoproductForm :: Generic (CoproductForm c1 c2 e a) _

derive instance newtypeCoproductForm :: Newtype (CoproductForm c1 c2 e a) _

derive instance eqCoproductForm ::
  ( Eq c1
  , Eq c2
  , Eq e
  , Eq a
  ) =>
  Eq (CoproductForm c1 c2 e a)

derive instance functorCoproductForm :: Functor (CoproductForm c1 c2 e)

derive newtype instance invariantCoproductForm ::
  Invariant (CoproductForm c1 c2 e)

derive newtype instance bifunctorCoproductForm ::
  Bifunctor (CoproductForm c1 c2)

derive newtype instance foldableCoproductForm ::
  Foldable (CoproductForm c1 c2 e)

derive newtype instance bifoldableCoproductForm ::
  Bifoldable (CoproductForm c1 c2)

instance arbitraryNewtypeCoproductContext ::
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
  Arbitrary (CoproductContext (f1 c1 e1 a) (f2 c2 e2 b)) where
  arbitrary = map (bimap fromForm fromForm)
    $ CoproductContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

derive instance genericCoproductContext :: Generic (CoproductContext c1 c2) _
derive instance eqCoproductContext ::
  ( Eq c1
  , Eq c2
  ) =>
  Eq (CoproductContext c1 c2)

derive instance ordCoproductContext ::
  ( Ord c1
  , Ord c2
  ) =>
  Ord (CoproductContext c1 c2)

derive instance functorCoproductContext :: Functor (CoproductContext c1)
instance bifunctorCoproductContext :: Bifunctor CoproductContext where
  bimap f g (CoproductContext initialChoice choice c1 c2) =
    CoproductContext initialChoice choice (f c1) (g c2)

instance showCoproductContext ::
  ( Show c1
  , Show c2
  ) =>
  Show (CoproductContext c1 c2) where
  show = genericShow

instance formContextCoproduct ::
  ( FormContext c1 i1 o1
  , FormContext c2 i2 o2
  , IsForm f1 c1 e1 a
  , IsForm f2 c2 e2 b
  ) =>
  FormContext
    (CoproductContext (f1 e1 a) (f2 e2 b))
    (Boolean /\ i1 /\ i2)
    (Result e1 a \/ Result e2 b) where
  current (CoproductContext _ choice f1 f2) =
    choice /\ current (toForm f1) /\ current (toForm f2)
  initial (CoproductContext choice _ f1 f2) =
    choice /\ initial (toForm f1) /\ initial (toForm f2)
  load (choice /\ i1 /\ i2) (CoproductContext _ _ f1 f2) =
    CoproductContext
      choice
      choice
      (fromForm $ load i1 $ toForm f1)
      (fromForm $ load i2 $ toForm f2)
  output (CoproductContext _ false f1 _) = Left $ output (toForm f1)
  output (CoproductContext _ true _ f2) = Right $ output (toForm f2)
  update (choice /\ i1 /\ i2) (CoproductContext initialChoice _ f1 f2) =
    CoproductContext
      initialChoice
      choice
      (fromForm $ update i1 $ toForm f1)
      (fromForm $ update i2 $ toForm f2)

instance formContextCoproductForm ::
  ( FormContext (CoproductContext c1 c2) i o
  ) =>
  FormContext (CoproductForm c1 c2 e a) i (Result e a) where
  current = current <<< viewContext <<< unwrap
  initial = initial <<< viewContext <<< unwrap
  load i = over CoproductForm $ overContext (load i)
  output = extractResult <<< unwrap
  update i = over CoproductForm $ overContext (update i)

lsetContext
  :: forall c1 c2 e a. c1 -> CoproductForm c1 c2 e a -> CoproductForm c1 c2 e a
lsetContext = loverContext <<< const

rsetContext
  :: forall c1 c2 e a. c2 -> CoproductForm c1 c2 e a -> CoproductForm c1 c2 e a
rsetContext = roverContext <<< const

bisetContext
  :: forall c1 c2 e a
   . c1
  -> c2
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
bisetContext c1 = bioverContext (const c1) <<< const

loverContext
  :: forall c1 c2 e a
   . (c1 -> c1)
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
loverContext = over CoproductForm <<< overContext <<< lmap

roverContext
  :: forall c1 c2 e a
   . (c2 -> c2)
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
roverContext = over CoproductForm <<< overContext <<< map

bioverContext
  :: forall c1 c2 e a
   . (c1 -> c1)
  -> (c2 -> c2)
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
bioverContext f1 = over CoproductForm <<< overContext <<< bimap f1

lload
  :: forall c1 c2 i1 i2 o e a
   . FormContext (CoproductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
lload i f = load (lmap (const i) $ current f) f

rload
  :: forall c1 c2 i1 i2 o e a
   . FormContext (CoproductForm c1 c2 e a) (i1 /\ i2) o
  => i2
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
rload i f = load (map (const i) $ current f) f

biload
  :: forall c1 c2 i1 i2 o e a
   . FormContext (CoproductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> i2
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
biload i1 i2 = load (i1 /\ i2)

lupdate
  :: forall c1 c2 i1 i2 o e a
   . FormContext (CoproductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
lupdate i f = update (lmap (const i) $ current f) f

rupdate
  :: forall c1 c2 i1 i2 o e a
   . FormContext (CoproductForm c1 c2 e a) (i1 /\ i2) o
  => i2
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
rupdate i f = update (map (const i) $ current f) f

biupdate
  :: forall c1 c2 i1 i2 o e a
   . FormContext (CoproductForm c1 c2 e a) (i1 /\ i2) o
  => i1
  -> i2
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
biupdate i1 i2 = update (i1 /\ i2)

biimapContext
  :: forall c1 c1' c2 c2' e a
   . (c1' -> c1)
  -> (c1 -> c1')
  -> (c2' -> c2)
  -> (c2 -> c2')
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1' c2' e a
biimapContext f g i j = over CoproductForm $ imapContext (bimap f i) (bimap g j)

choose
  :: forall c1 c2 e a
   . Boolean
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
choose choice = over CoproductForm $ overContext go
  where
  go (CoproductContext initialChoice _ c1 c2) =
    CoproductContext initialChoice choice c1 c2

loadChoice
  :: forall c1 c2 e a
   . Boolean
  -> CoproductForm c1 c2 e a
  -> CoproductForm c1 c2 e a
loadChoice choice = over CoproductForm $ overContext go
  where
  go (CoproductContext _ _ c1 c2) =
    CoproductContext choice choice c1 c2
