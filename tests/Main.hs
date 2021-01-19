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
            withAcquire (makeAcquire (pure ()) (\() -> writeIORef ref 1)) $ \_ -> pure ()
            value <- readIORef ref
            value `shouldBe` 1
        it "Should run the handler on exceptions" $ do
            ref <- newIORef 0
            result <- try $ withAcquire (makeAcquire (pure ()) (\() -> writeIORef ref 1)) $ \() ->
                throwString "Error"
            case result of
                Right () -> error "Should have thrown an exception"
                Left (_ :: SomeException) -> do
                    value <- readIORef ref
                    value `shouldBe` 1
        it "Should run cleanup handlers in reverse order" $ do
            ref <- newIORef []
            withAcquire
                (do
                    makeAcquire
                        (pure ())
                        (\() -> modifyIORef ref (<>[1]))
                    makeAcquire
                        (pure ())
                        (\() -> modifyIORef ref (<>[2]))
                    makeAcquire
                        (pure ())
                        (\() -> modifyIORef ref (<>[3]))
                    pure ()
                )
                $ \() -> pure ()
            value <- readIORef ref
            value `shouldBe` [3,2,1]
