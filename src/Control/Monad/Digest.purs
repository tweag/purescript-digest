module Control.Monad.Digest
  ( Digest
  , digest
  , runDigest
  , evalDigest
  , execDigest
  , mapDigest
  , withDigest
  , module DigestT
  ) where

import Prelude

import Control.Monad.Digest.Trans
  ( DigestResult
  , DigestT(..)
  , evalDigestT
  , execDigestT
  , hoistDigestT
  , mapDigestT
  , mapResult
  , mapView
  , runDigestT
  , withDigestT
  ) as DigestT
import Control.Monad.Digest.Trans
  ( DigestResult
  , DigestT(..)
  , evalDigestT
  , execDigestT
  , mapDigestT
  , runDigestT
  , withDigestT
  )
import Data.Identity (Identity(..))
import Data.Maybe (Maybe)
import Data.Newtype (over, unwrap)

-- | A synonym for the `DigestT` monad transformer applied to the `Identity`
-- | monad.
type Digest view input result = DigestT view input Identity result

digest :: forall v i a. (i -> DigestResult v a) -> Digest v i a
digest f = DigestT \i -> pure (f i)

-- | Run a computation in the `Digest` monad.
runDigest :: forall v i a. Digest v i a -> i -> DigestResult v a
runDigest d i = unwrap (runDigestT d i)

-- | Run a computation in the `Digest` monad, discarding the view.
evalDigest :: forall v i a. Digest v i a -> i -> Maybe a
evalDigest d i = unwrap (evalDigestT d i)

-- | Run a computation in the `Digest` monad, discarding the result.
execDigest :: forall v i a. Digest v i a -> i -> v
execDigest d i = unwrap (execDigestT d i)

-- | Change view and result types of a `Digest` monad action.
mapDigest
  :: forall v w i a b
   . (DigestResult v a -> DigestResult w b)
  -> Digest v i a
  -> Digest w i b
mapDigest = mapDigestT <<< over Identity

-- | Change the input type of a `Digest` monad action.
withDigest
  :: forall v i j a
   . (j -> i)
  -> Digest v i a
  -> Digest v j a
withDigest = withDigestT
