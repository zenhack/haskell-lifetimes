{-# LANGUAGE NamedFieldPuns #-}
module Lifetimes.Rc
    ( Rc
    , addRef
    , refCounted
    ) where

import Control.Concurrent.STM
import Lifetimes
import Zhp

data Rc a = Rc
    { count   :: TVar Int
    , value   :: a
    , cleanup :: IO ()
    }

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
    let value = getResource res
    cleanup <- detach res
    count <- newTVar 1
    pure Rc { count, cleanup, value }


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
