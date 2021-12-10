module Data.Form.Coproduct
  ( CoproductForm(..)
  , CoproductContext
  , coproduct
  , loadChoice
  , overChoice
  , updateChoice
  , viewChoice
  , viewInitialChoice
  ) where

import Prelude

import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (class Foldable)
import Data.Form
  ( class FormContext
  , class IsForm
  , Form
  , ctx_current
  , ctx_initial
  , ctx_load
  , ctx_output
  , ctx_update
  , form
  , fromForm
  , overContext
  , toForm
  , viewContext
  )
import Data.Form.Result (Result, ignore)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong ((&&&))
import Data.String (joinWith)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)

-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------

data CoproductContext cl cr = CoproductContext Boolean Boolean cl cr

newtype CoproductForm cl cr e a = CoproductForm
  (Form (CoproductContext cl cr) e a)

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

coproduct
  :: forall f1 f2 cl cr i1 i2 o1 o2 e1 e2 a b e
   . FormContext cl i1 o1
  => FormContext cr i2 o2
  => IsForm f1 cl e1 a
  => IsForm f2 cr e2 b
  => Boolean
  -> f1 e1 a
  -> f2 e2 b
  -> CoproductForm (f1 e1 a) (f2 e2 b) e (a \/ b)
coproduct choice f1 f2 =
  form (CoproductContext choice choice f1 f2) validate
  where
  validate (Left a) = Left <$> ignore a
  validate (Right b) = Right <$> ignore b

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

derive instance genericCoproductForm :: Generic (CoproductForm cl cr e a) _

derive instance newtypeCoproductForm :: Newtype (CoproductForm cl cr e a) _

derive instance eqCoproductForm ::
  ( Eq cl
  , Eq cr
  , Eq e
  , Eq a
  ) =>
  Eq (CoproductForm cl cr e a)

derive instance functorCoproductForm :: Functor (CoproductForm cl cr e)

derive newtype instance invariantCoproductForm ::
  Invariant (CoproductForm cl cr e)

derive newtype instance bifunctorCoproductForm ::
  Bifunctor (CoproductForm cl cr)

derive newtype instance foldableCoproductForm ::
  Foldable (CoproductForm cl cr e)

derive newtype instance bifoldableCoproductForm ::
  Bifoldable (CoproductForm cl cr)

instance arbitraryNewtypeCoproductContext ::
  ( Arbitrary cl
  , Arbitrary cr
  , Arbitrary i1
  , Arbitrary i2
  , Arbitrary e1
  , Arbitrary e2
  , Arbitrary a
  , Arbitrary b
  , Coarbitrary o1
  , Coarbitrary o2
  , FormContext cl i1 o1
  , FormContext cr i2 o2
  , IsForm (f1 cl) cl e1 a
  , IsForm (f2 cr) cr e2 b
  ) =>
  Arbitrary (CoproductContext (f1 cl e1 a) (f2 cr e2 b)) where
  arbitrary = map (bimap fromForm fromForm)
    $ CoproductContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

derive instance eqCoproductContext ::
  ( Eq cl
  , Eq cr
  ) =>
  Eq (CoproductContext cl cr)

derive instance ordCoproductContext ::
  ( Ord cl
  , Ord cr
  ) =>
  Ord (CoproductContext cl cr)

derive instance functorCoproductContext :: Functor (CoproductContext cl)
instance bifunctorCoproductContext :: Bifunctor CoproductContext where
  bimap f g (CoproductContext initialChoice choice cl cr) =
    CoproductContext initialChoice choice (f cl) (g cr)

instance showCoproductContext ::
  ( Show cl
  , Show cr
  ) =>
  Show (CoproductContext cl cr) where
  show (CoproductContext initialChoice choice cl cr) =
    ("(" <> _)
      $ (_ <> ")")
      $ joinWith " "
          [ "CoproductContext"
          , show initialChoice
          , show choice
          , show cl
          , show cr
          ]

instance formContextCoproduct ::
  ( FormContext cl i1 o1
  , FormContext cr i2 o2
  , IsForm f1 cl e1 a
  , IsForm f2 cr e2 b
  ) =>
  FormContext
    (CoproductContext (f1 e1 a) (f2 e2 b))
    (Boolean /\ i1 /\ i2)
    (Result e1 a \/ Result e2 b) where
  ctx_current (CoproductContext _ choice f1 f2) =
    choice /\ ctx_current (toForm f1) /\ ctx_current (toForm f2)
  ctx_initial (CoproductContext choice _ f1 f2) =
    choice /\ ctx_initial (toForm f1) /\ ctx_initial (toForm f2)
  ctx_load (choice /\ i1 /\ i2) (CoproductContext _ _ f1 f2) =
    CoproductContext
      choice
      choice
      (fromForm $ ctx_load i1 $ toForm f1)
      (fromForm $ ctx_load i2 $ toForm f2)
  ctx_output (CoproductContext _ false f1 _) = Left $ ctx_output (toForm f1)
  ctx_output (CoproductContext _ true _ f2) = Right $ ctx_output (toForm f2)
  ctx_update (choice /\ i1 /\ i2) (CoproductContext initialChoice _ f1 f2) =
    CoproductContext
      initialChoice
      choice
      (fromForm $ ctx_update i1 $ toForm f1)
      (fromForm $ ctx_update i2 $ toForm f2)

viewChoice :: forall cl cr e a. CoproductForm cl cr e a -> Boolean
viewChoice = viewContext >>> \(CoproductContext _ choice _ _) -> choice

viewInitialChoice :: forall cl cr e a. CoproductForm cl cr e a -> Boolean
viewInitialChoice = viewContext >>> \(CoproductContext choice _ _ _) -> choice

overChoice
  :: forall cl cr e a
   . (Boolean -> Boolean)
  -> CoproductForm cl cr e a
  -> CoproductForm cl cr e a
overChoice f = uncurry updateChoice <<< (f <<< viewChoice &&& identity)

updateChoice
  :: forall cl cr e a
   . Boolean
  -> CoproductForm cl cr e a
  -> CoproductForm cl cr e a
updateChoice choice =
  overContext \(CoproductContext initialChoice _ cl cr) ->
    CoproductContext initialChoice choice cl cr

loadChoice
  :: forall cl cr e a
   . Boolean
  -> CoproductForm cl cr e a
  -> CoproductForm cl cr e a
loadChoice choice =
  overContext \(CoproductContext _ _ cl cr) ->
    CoproductContext choice choice cl cr
