-- | Module: Lifetimes.Async
-- Description: Lifteimes integration for the async package.
module Lifetimes.Async (acquireAsync) where

import Control.Concurrent.Async (Async, async, cancel, wait)
import Lifetimes
import Zhp

-- | Spawn an async task. When it is time to reclaim the resource, 'cancel'
-- will be called.
acquireAsync :: IO a -> Acquire (Async a)
acquireAsync io = mkAcquire (async io) cancel
