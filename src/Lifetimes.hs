{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
    , mustGetResource
    , releaseEarly
    , detach
    ) where

import           Control.Concurrent.STM
import           Control.Exception          (Exception, bracket, finally)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Foldable              (fold)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import           Zhp

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

data Lifetime = Lifetime
    { resources      :: TVar (M.Map ReleaseKey Cleanup)
    , nextReleaseKey :: TVar ReleaseKey
    }

data Resource a = Resource
    { releaseKey :: TVar ReleaseKey
    , lifetime   :: TVar Lifetime
    , valueCell  :: TVar (Maybe a)
    }

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
    modifyTVar (resources lt) $ M.insert key clean
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

currentLifetime :: Acquire Lifetime
currentLifetime = Acquire ask

mkAcquire :: IO a -> (a -> IO ()) -> Acquire a
mkAcquire get clean = Acquire $ do
    lt <- ask
    fst <$> liftIO (acquire1 lt get clean)

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
    value <- fromJust <$> atomically (getResource res)
    use value

withLifetime :: (Lifetime -> IO a) -> IO a
withLifetime = bracket createLifetime destroyLifetime

acquire :: Lifetime -> Acquire a -> IO (Resource a)
acquire lt (Acquire acq) = do
    (lt', res) <- acquire1 lt createLifetime destroyLifetime
    value' <- runReaderT acq lt'
    valueCell <- atomically $ newTVar $ Just value'
    pure res { valueCell }

acquireValue :: Lifetime -> Acquire a -> IO a
acquireValue lt acq = do
    res <- acquire lt acq
    fromJust <$> atomically (getResource res)

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

getResource :: Resource a -> STM (Maybe a)
getResource r = readTVar (valueCell r)

mustGetResource :: Resource a -> STM a
mustGetResource r = getResource r >>= \case
    Nothing -> throwSTM ResourceExpired
    Just v  -> pure v

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
