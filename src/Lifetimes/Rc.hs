{-# LANGUAGE NamedFieldPuns #-}
-- | Support for working with reference-counted resources.
--
-- Rather than associating a resource with one lifetime, a reference counted
-- resource associates each *reference* with a lifetime, and is released when
-- all references have expired.
module Lifetimes.Rc
    ( Rc
    , addRef
    , refCounted
    ) where

import Control.Concurrent.STM
import Lifetimes
import Zhp

-- | A resource which is managed by reference counting.
data Rc a = Rc
    { count   :: TVar Int
    , value   :: a
    , cleanup :: IO ()
    }

-- | Acquire a new reference.
addRef :: Rc a -> Acquire a
addRef rc =
    mkAcquire
        (atomically $ incRef rc)
        (\_ -> join $ atomically $ decRef rc)

acquireResource :: Acquire a -> Acquire (Resource a)
acquireResource acq = do
    lt <- currentLifetime
    mkAcquire
        (acquire lt acq)
        releaseEarly

resourceToRc :: Resource a -> STM (Rc a)
resourceToRc res = do
    value <- mustGetResource res
    cleanup <- detach res
    count <- newTVar 1
    pure Rc { count, cleanup, value }


-- | Acquire a resource using refcounting. Takes an 'Acquire' for the underlying
-- resource, and returns one that acquires an initial reference to it. Additional
-- references may be created using 'addRef', and the underlying resource will be
-- kept alive until all resources are released.
refCounted :: Acquire a -> Acquire (Rc a)
refCounted acq = do
    res <- acquireResource acq
    mkAcquire
        (atomically $ resourceToRc res)
        (join . atomically . decRef)

incRef :: Rc a -> STM a
incRef Rc{count, value} = do
    modifyTVar' count succ
    pure value

decRef :: Rc a -> STM (IO ())
decRef Rc{count, cleanup} = do
    modifyTVar' count pred
    c <- readTVar count
    pure $ case c of
        0 -> cleanup
        _ -> pure ()
