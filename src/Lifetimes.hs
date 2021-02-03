{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Lifetimes
    ( Acquire
    , Resource
    , Lifetime
    , mkAcquire
    , currentLifetime
    , newLifetime
    , withAcquire
    , withLifetime
    , acquire
    , acquireValue
    , moveTo
    , moveToSTM
    , getResource
    , releaseEarly
    , detach
    ) where

import           Control.Concurrent.STM
import           Control.Exception          (bracket, finally)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Foldable              (fold)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import           Zhp

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

data Lifetime = Lifetime
    { resources      :: TVar (M.Map ReleaseKey Cleanup)
    , nextReleaseKey :: TVar ReleaseKey
    }

data Resource a = Resource
    { releaseKey :: TVar ReleaseKey
    , lifetime   :: TVar Lifetime
    , value      :: a
    }
    deriving(Functor)

newtype Acquire a = Acquire (ReaderT Lifetime IO a)
    deriving(Functor, Applicative, Monad, MonadIO)

newReleaseKey :: Lifetime -> STM ReleaseKey
newReleaseKey Lifetime{nextReleaseKey} = do
    key <- readTVar nextReleaseKey
    writeTVar nextReleaseKey $! succ key
    pure key

acquire1 :: Lifetime -> IO a -> (a -> IO ()) -> IO (Resource a)
acquire1 lt@Lifetime{resources} get clean = do
    bracket
        (get >>= newTVarIO . Just)
        (\var -> atomically (readTVar var) >>= traverse_ clean)
        (\var -> atomically $ do
            key <- newReleaseKey lt
            value <- fromJust <$> readTVar var
            modifyTVar resources $
                M.insert key (Cleanup (clean value))
            writeTVar var Nothing
            lifetime <- newTVar lt
            releaseKey <- newTVar key
            pure Resource
                { releaseKey
                , lifetime
                , value
                }
        )

currentLifetime :: Acquire Lifetime
currentLifetime = Acquire ask

mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquire get clean = Acquire $ do
    lt <- ask
    fmap getResource . liftIO $ acquire1 lt get clean

newLifetime :: Acquire Lifetime
newLifetime = mkAcquire createLifetime destroyLifetime

createLifetime :: IO Lifetime
createLifetime = Lifetime
    <$> newTVarIO M.empty
    <*> newTVarIO minBound

destroyLifetime :: Lifetime -> IO ()
destroyLifetime Lifetime{resources} =
    join $ atomically $ do
        clean <- fold <$> readTVar resources
        writeTVar resources $! M.empty
        pure $ runCleanup clean

withAcquire :: Acquire a -> (a -> IO b) -> IO b
withAcquire acq use = withLifetime $ \lt -> do
    res <- acquire lt acq
    use (getResource res)

withLifetime :: (Lifetime -> IO a) -> IO a
withLifetime = bracket createLifetime destroyLifetime

acquire :: Lifetime -> Acquire a -> IO (Resource a)
acquire lt (Acquire acq) = do
    lt' <- acquire1 lt createLifetime destroyLifetime
    value' <- runReaderT acq (value lt')
    pure lt' { value = value' }

acquireValue :: Lifetime -> Acquire a -> IO a
acquireValue lt acq = getResource <$> acquire lt acq

moveTo :: Resource a -> Lifetime -> IO ()
moveTo r l = atomically $ moveToSTM r l

moveToSTM :: Resource a -> Lifetime -> STM ()
moveToSTM r newLifetime = do
    oldKey <- readTVar $ releaseKey r
    oldLifetime <- readTVar $ lifetime r
    oldMap <- readTVar $ resources oldLifetime
    case M.lookup oldKey oldMap of
        Nothing -> pure () -- already freed.
        Just clean -> do
            modifyTVar (resources oldLifetime) $ M.delete oldKey
            newKey <- newReleaseKey newLifetime
            writeTVar (releaseKey r) $! newKey
            modifyTVar (resources newLifetime) $ M.insert newKey clean

releaseEarly :: Resource a -> IO ()
releaseEarly r =
    bracket
        (atomically (detach r))
        id
        (\_ -> pure ())

getResource :: Resource a -> a
getResource = value

-- | Detach the resource from its lifetime, returning the cleanup handler.
-- NOTE: if the caller does not otherwise arrange to run the cleanup handler,
-- it will *not* be executed.
detach :: Resource a -> STM (IO ())
detach r = do
    key <- readTVar $ releaseKey r
    lt <- readTVar $ lifetime r
    ltMap <- readTVar $ resources lt
    let result = M.lookup key ltMap
    for_ result $ \_ ->
        modifyTVar (resources lt) $ M.delete key
    pure $ traverse_ runCleanup result
