{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main (main) where

import           Control.Exception.Safe (SomeException, throwString, try)
import           Data.IORef
import           Lifetimes
import qualified Lifetimes.Gc           as Gc
import qualified Lifetimes.Rc           as Rc
import           System.Mem             (performGC)
import           Test.Hspec
import           Zhp

main :: IO ()
main = hspec $ do
    describe "withAcquire" $ do
        it "Should run the handler on success" $ do
            ref <- newIORef 0
            withAcquire (mkAcquire (pure ()) (\() -> writeIORef ref 1)) $ \_ -> pure ()
            value <- readIORef ref
            value `shouldBe` 1
        it "Should run the handler on exceptions" $ do
            ref <- newIORef 0
            result <- try $ withAcquire (mkAcquire (pure ()) (\() -> writeIORef ref 1)) $ \() ->
                throwString "Error"
            case result of
                Right () -> error "Should have thrown an exception"
                Left (_ :: SomeException) -> do
                    value <- readIORef ref
                    value `shouldBe` 1
        it "Should run cleanup handlers in reverse order" $ do
            ref <- newIORef []
            withAcquire
                (traverse_ (append ref) [1,2,3])
                pure
            value <- readIORef ref
            value `shouldBe` [3,2,1]
    describe "nested lifetimes" $ do
        it "Should order resources underneath their lifetimes." $ do
            ref <- newIORef []
            withLifetime $ \lt -> do
                void $ acquire lt $ append ref 1
                lt' <- acquireValue lt newLifetime
                void $ acquire lt $ append ref 2
                -- even though 3 is allocated after 2, it will be freed when
                -- lt' is freed.
                void $ acquire lt' $ append ref 3
            value <- readIORef ref
            value `shouldBe` [2,3,1]
    describe "releaseEarly" $ do
        it "Should release the resource immediately" $ do
            ref <- newIORef []
            withLifetime $ \lt -> do
                void $ acquire lt $ append ref 1
                res2 <- acquire lt $ append ref 2
                void $ acquire lt $ append ref 3
                releaseEarly res2
            value <- readIORef ref
            value `shouldBe` [2,3,1]
    describe "moveTo" $ do
        it "Should live longer when moved to a longer-lived lifetime" $ do
            ref <- newIORef []
            withLifetime $ \lt -> do
                void $ acquire lt $ append ref 1
                lt' <- acquireValue lt newLifetime
                void $ acquire lt $ append ref 2
                res3 <- acquire lt' $ append ref 3
                -- If we didn't move this, it would be freed when lt'
                -- is freed, but this will make it live until the end of
                -- lt instead.
                moveTo res3 lt
            value <- readIORef ref
            value `shouldBe` [3,2,1]
        it "Should do nothing if the resource has already been freed" $ do
            ref <- newIORef []
            withLifetime $ \lt -> do
                res1 <- acquire lt $ append ref 1
                lt' <- acquireValue lt newLifetime
                void $ acquire lt $ append ref 2
                void $ acquire lt' $ append ref 3
                releaseEarly res1
                moveTo res1 lt'
            value <- readIORef ref
            value `shouldBe` [1,2,3]
    describe "Lifetimes.Rc" $ do
        it "Should release the resources in order if no extra references are acquired" $ do
            ref <- newIORef []
            withLifetime $ \lt -> do
                void $ acquire lt $ Rc.refCounted $ append ref 1
                void $ acquire lt $ Rc.refCounted $ append ref 2
            value <- readIORef ref
            value `shouldBe` [2,1]
        it "Should last until its final reference is dropped" $ do
            ref <- newIORef []
            withLifetime $ \lt1 -> do
                void $ withLifetime $ \lt2 -> do
                    res <- acquireValue lt2 $ Rc.refCounted $ append ref 1
                    acquire lt1 $ Rc.addRef res
                value <- readIORef ref
                value `shouldBe` []
            value <- readIORef ref
            value `shouldBe` [1]
    describe "Lifetimes.Gc" $ do
        it "Should not release the resource while an acquired reference is active." $ do
            ref <- newIORef []
            cell <- Gc.newCell ()
            Gc.addFinalizer cell (modifyIORef ref (<>[1]))
            withAcquire (Gc.acquireCell cell) $ \() -> do
                performGC
                value <- readIORef ref
                value `shouldBe` []
        describe "moveToGC" $ do
            it "Should not release a resource when its original lifetime is up." $ do
                ref <- newIORef []
                cell <- withLifetime $ \lt -> do
                    res <- acquire lt $ append ref 1
                    Gc.moveToGc res
                value <- readIORef ref
                value `shouldBe` []
                -- Touch the value to prevent GC:
                withAcquire (Gc.acquireCell cell) $ \_ -> pure ()

append :: IORef [Int] -> Int -> Acquire ()
append ref n = mkAcquire
    (pure ())
    (\() -> modifyIORef ref (<>[n]))
