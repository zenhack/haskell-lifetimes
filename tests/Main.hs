{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception.Safe (SomeException, throwString, try)
import Data.IORef
import Lifetimes
import Test.Hspec
import Zhp

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
                acquire lt $ append ref 1
                lt' <- acquireValue lt newLifetime
                acquire lt $ append ref 2
                -- even though 3 is allocated after 2, it will be freed when
                -- lt' is freed.
                acquire lt' $ append ref 3
            value <- readIORef ref
            value `shouldBe` [2,3,1]
    describe "moveTo" $ do
        it "Should live longer when moved to a longer-lived lifetime" $ do
            ref <- newIORef []
            withLifetime $ \lt -> do
                acquire lt $ append ref 1
                lt' <- acquireValue lt newLifetime
                acquire lt $ append ref 2
                res3 <- acquire lt' $ append ref 3
                -- If we didn't move this, it would be freed when lt'
                -- is freed, but this will make it live until the end of
                -- lt instead.
                moveTo res3 lt
            value <- readIORef ref
            value `shouldBe` [3,2,1]

append :: IORef [Int] -> Int -> Acquire ()
append ref n = mkAcquire
    (pure ())
    (\() -> modifyIORef ref (<>[n]))
