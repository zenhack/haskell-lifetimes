{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
-- | Module: Lifetimes
-- Description: Flexible resource management using first class lifetimes.
--
-- This package is centered around a couple types:
--
-- * 'Acquire' is a monadic context in which resources can be acquired.
--   These can be executed using 'acquire', or for simpler cases 'withAcquire'
--   or 'acquireValue'.
-- * 'Resource' is a handle to a resource. The value for the resource can
--   be read from this, and the 'Resource' can also be used to manipulate
--   the resource's lifetime.
-- * 'Liftime' is the type of first-class liftimes; resources are attached
--   to these and can be moved between them.
module Lifetimes
    (
    -- * Lifetimes
      Lifetime
    , newLifetime
    , withLifetime

    -- * Acquiring resources
    , Acquire
    , mkAcquire
    , withAcquire
    , acquire
    , acquireValue
    , currentLifetime

    -- * Using resources
    , Resource
    , getResource
    , mustGetResource

    -- * Releasing resources
    , releaseEarly
    , detach

    -- * Move semantics
    , moveTo

    -- * Errors
    , ResourceExpired(..)
    ) where

import           Control.Concurrent.STM
import           Control.Exception          (Exception, bracket, finally)
import           Control.Monad.STM.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Foldable              (fold)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import           Zhp

-- | Error thrown when an attempt is made to use an expired
-- resource or lifetime.
data ResourceExpired = ResourceExpired
    deriving(Show, Read, Ord, Eq)
instance Exception ResourceExpired

newtype ReleaseKey = ReleaseKey Word64
    deriving(Show, Read, Ord, Eq, Bounded)

instance Enum ReleaseKey where
    toEnum n = ReleaseKey (toEnum n)
    fromEnum (ReleaseKey n) = fromEnum n

newtype Cleanup = Cleanup { runCleanup :: IO () }

instance Semigroup Cleanup where
    -- We want resources to be released in the opposite order from their
    -- acquisition, so x <> y releases y and then x.
    Cleanup x <> Cleanup y = Cleanup $ y `finally` x

instance Monoid Cleanup where
    mempty = Cleanup $ pure ()

-- | A 'Lifetime' is a represents the scope in which a 'Resource' is valid;
-- resources are attached to a lifetime when they are acquired, and will
-- be released when the lifetime ends.
data Lifetime = Lifetime
    { resources      :: TVar (Maybe (M.Map ReleaseKey Cleanup))
    , nextReleaseKey :: TVar ReleaseKey
    }

-- | Represents a resource with type @a@, which has a lifetime and an
-- associated cleanup handler.
data Resource a = Resource
    { releaseKey :: TVar ReleaseKey
    , lifetime   :: TVar Lifetime
    , valueCell  :: TVar (Maybe a)
    }

-- | An 'Acquire' is a monadic action that acquires some number of resources,
-- and registers cleanup handlers to be executed when their lifetime expires.
newtype Acquire a = Acquire (ReaderT Lifetime IO a)
    deriving(Functor, Applicative, Monad, MonadIO)

newReleaseKey :: Lifetime -> STM ReleaseKey
newReleaseKey Lifetime{nextReleaseKey} = do
    key <- readTVar nextReleaseKey
    writeTVar nextReleaseKey $! succ key
    pure key

addCleanup :: Lifetime -> Cleanup -> STM ReleaseKey
addCleanup lt clean = do
    key <- newReleaseKey lt
    modifyMaybeTVar (resources lt) $ M.insert key clean
    pure key

acquire1 :: Lifetime -> IO a -> (a -> IO ()) -> IO (a, Resource a)
acquire1 lt@Lifetime{resources} get clean = do
    bracket
        (get >>= newTVarIO . Just)
        (\var -> atomically (readTVar var) >>= traverse_ clean)
        (\var -> atomically $ do
            value <- fromJust <$> readTVar var
            key <- addCleanup lt $ Cleanup (clean value)
            writeTVar var Nothing
            lifetime <- newTVar lt
            releaseKey <- newTVar key
            valueCell <- newTVar $ Just value
            pure
                ( value
                , Resource
                    { releaseKey
                    , lifetime
                    , valueCell
                    }
                )
        )

-- | Get the lifetime for the resources being acquired.
currentLifetime :: Acquire Lifetime
currentLifetime = Acquire ask

-- | @'mkAcquire' get cleanup@ acquires a resource with @get@, which will
-- be released by calling @cleanup@ when its lifetime ends.
mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquire get cleanup = Acquire $ do
    lt <- ask
    fst <$> liftIO (acquire1 lt get cleanup)

-- | Acquire a new lifetime, as its own resource. This allows creating
-- sub-groups of resources, which can be later moved as a unit.
newLifetime :: Acquire Lifetime
newLifetime = mkAcquire createLifetime destroyLifetime

createLifetime :: IO Lifetime
createLifetime = Lifetime
    <$> newTVarIO (Just M.empty)
    <*> newTVarIO minBound

modifyMaybeTVar :: TVar (Maybe a) -> (a -> a) -> STM ()
modifyMaybeTVar tvar f = do
    content <- readTVar tvar
    case content of
        Just v  -> writeTVar tvar $ Just $! f v
        Nothing -> throwSTM ResourceExpired

getResourceMap :: Lifetime -> STM (M.Map ReleaseKey Cleanup)
getResourceMap lt =
    readTVar (resources lt) >>= \case
        Just m  -> pure m
        Nothing -> throwSTM ResourceExpired

destroyLifetime :: Lifetime -> IO ()
destroyLifetime lt =
    join $ atomically $ do
        clean <- fold <$> getResourceMap lt
        writeTVar (resources lt) Nothing
        pure $ runCleanup clean

-- | 'withAcquire' acuires a resource, uses it, and then releases it.
-- @'withAcquire' ('mkAcquire' get cleanup)@ is equivalent to
-- @'bracket' get cleanup@.
withAcquire :: Acquire a -> (a -> IO b) -> IO b
withAcquire acq use = withLifetime $ \lt -> do
    res <- acquire lt acq
    value <- fromJust <$> atomically (getResource res)
    use value

-- | Execute an IO action within the scope of a newly allocated lifetime,
-- which ends when the IO action completes.
withLifetime :: (Lifetime -> IO a) -> IO a
withLifetime = bracket createLifetime destroyLifetime

-- | Acquire a resource, attaching it to the supplied lifetime.
acquire :: Lifetime -> Acquire a -> IO (Resource a)
acquire lt (Acquire acq) = do
    (lt', res) <- acquire1 lt createLifetime destroyLifetime
    value' <- runReaderT acq lt'
    valueCell <- atomically $ newTVar $ Just value'
    pure res { valueCell }

-- | Like 'acquire', but returns the value, rather than a 'Resource' wrapper.
-- conveinent when you don't need to move the resource or release it before
-- the lifetime expires.
acquireValue :: Lifetime -> Acquire a -> IO a
acquireValue lt acq = do
    res <- acquire lt acq
    fromJust <$> atomically (getResource res)

-- | Move a resource to another lifetime. The resource will be detached from
-- its existing lifetime, and so may live past it, but will be released when
-- the new lifetime expires.
moveTo :: MonadSTM m => Resource a -> Lifetime -> m ()
moveTo r newLifetime = liftSTM $ do
    oldKey <- readTVar $ releaseKey r
    oldLifetime <- readTVar $ lifetime r
    oldMap <- getResourceMap oldLifetime
    case M.lookup oldKey oldMap of
        Nothing -> pure () -- already freed.
        Just clean -> do
            modifyMaybeTVar (resources oldLifetime) $ M.delete oldKey
            newKey <- newReleaseKey newLifetime
            writeTVar (releaseKey r) $! newKey
            modifyMaybeTVar (resources newLifetime) $ M.insert newKey clean

-- | Release a resource early, before its lifetime would otherwise end.
releaseEarly :: Resource a -> IO ()
releaseEarly r =
    bracket
        (atomically takeValue)
        releaseValue
        (\_ -> pure ())
  where
    takeValue = do
        v <- getResource r
        writeTVar (valueCell r) Nothing
        pure v
    releaseValue v =
        for_ v $ \_ ->
            join $ atomically (detach r)

-- | Get the value associated with a resource, returning 'Nothing' if the
-- resource's lifetime is expired.
getResource :: MonadSTM m => Resource a -> m (Maybe a)
getResource r = liftSTM $ readTVar (valueCell r)

-- | Like 'getResource', but throws a 'ResourceExpired' exception instead
-- of returning a 'Maybe'.
mustGetResource :: MonadSTM m => Resource a -> m a
mustGetResource r = liftSTM $ getResource r >>= \case
    Nothing -> throwSTM ResourceExpired
    Just v  -> pure v

-- | Detach the resource from its lifetime, returning the cleanup handler.
-- NOTE: if the caller does not otherwise arrange to run the cleanup handler,
-- it will *not* be executed.
detach :: MonadSTM m => Resource a -> m (IO ())
detach r = liftSTM $ do
    key <- readTVar $ releaseKey r
    lt <- readTVar $ lifetime r
    ltMap <- getResourceMap lt
    let result = M.lookup key ltMap
    for_ result $ \_ ->
        modifyMaybeTVar (resources lt) $ M.delete key
    pure $ traverse_ runCleanup result
