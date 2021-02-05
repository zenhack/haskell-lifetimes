-- | Module: Lifetimes.Gc
-- Description: Attach garbage-collector managed finalizers to resources.
--
-- This module integrates the lifetimes package with GHC's finalizers; this
-- allows you to have the GC run cleanup actions when a resource is garbage
-- collected, rather than managing its lifetime explicitly.
--
-- You should think twice before using this; much of the point of this package
-- is to manage resources whose lifetime is *semantically significant*, so
-- in many cases you will want more control over when the resource is released
-- than this module provides. It would be inappropriate to use this
-- if:
--
-- * You need the resource to be cleaned up promptly for semantic reasons
--   (e.g. dropping a network connection).
-- * The resource is scarce (e.g. file descriptors), so it is not safe to
--   wait for the garbage collector to get around it.
--
-- It is sometimes appropriate however, when time of release is mostly an
-- implementation detail. In particular, this module is fine for use cases
-- where you would want to use a finalizer anyway, and it can be safer:
-- The GHC APIs allow you to attach finalizers to arbitrary values, but
-- doing so is perlious; the compiler and runtime system are free to do
-- many transformations on the code that uses pure values, so it is easy
-- to end up with the finalizer being run sooner than you intended. This
-- module provides a 'Cell' type for finalizable values which is easier
-- to reason about.
{-# LANGUAGE NamedFieldPuns #-}
module Lifetimes.Gc
    ( Cell
    , readCell
    , moveToGc
    , newCell
    , addFinalizer
    ) where

import Control.Concurrent.MVar (MVar, mkWeakMVar, newEmptyMVar)
import Control.Concurrent.STM
import Control.Exception       (mask)
import Lifetimes
import Zhp

-- | A cell, containing a value with possible finalizers attached. This differs
-- from 'Resource' in that getting the underlying value cannot fail, since
-- cleanup is controlled by the garbage collector.
newtype Cell a
    = Cell (TVar (CellData a))
    deriving(Eq)

-----------------------------------------------------------------------
-- Implementation notes:
--
-- From the docs for the 'Weak' type:
--
-- > WARNING: weak pointers to ordinary non-primitive Haskell types
-- > are particularly fragile, because the compiler is free to optimise
-- > away or duplicate the underlying data structure. Therefore
-- > attempting to place a finalizer on an ordinary Haskell type may
-- > well result in the finalizer running earlier than you expected.
-- >
-- > [...]
-- >
-- > Finalizers can be used reliably for types that are created
-- > explicitly and have identity, such as IORef and MVar. [...]
--
-- So instead, we provide a 'Cell' type, which:
--
-- * Wraps simple value
-- * Can be created and read inside STM, and
-- * May safely have finalizers, using the 'addFinalizer' function in
--   this module.
-- * Ensures that the finalizers will not be run before any transaction that
--   reads data is complete.
--
-- Note that it is *not* safe to use the primitives from "Sys.Mem.Weak" to
-- add finalizers.
-----------------------------------------------------------------------

-- The actual contents of a cell. This is wrapped in a 'TVar' to force accesses
-- to add the a reference the transaction log from which the finalizers are
-- reachable, thus preventing them from running before the completion of any
-- transaction that examines the value.
data CellData a = CellData
    { value      :: a
    -- ^ The value wrapped by the cell.

    , finalizers :: [MVar ()]
    -- ^ Experimentally, TVars appear not to be safe for finalizers, so
    -- instead we create MVars for the finalizers, and store them in this
    -- list so that we maintain a reference to them.
    }
    deriving(Eq)

-- | Get the value from a cell. The value will not be collected until after
-- the all transactions which read it complete.
readCell :: Cell a -> STM a
readCell (Cell state) = value <$> readTVar state

-- | Create  a new cell, initially with no finalizers.
newCell :: a -> STM (Cell a)
newCell value = Cell <$> newTVar CellData { value, finalizers = [] }

-- | Add a new finalizer to the cell. Cells may have many finalizers
-- attached.
addFinalizer :: Cell a -> IO () -> IO ()
addFinalizer (Cell stateVar) fin = do
    mvar <- newEmptyMVar
    _ <- mkWeakMVar mvar fin
    atomically $ modifyTVar' stateVar $ \state@CellData{finalizers} ->
        state { finalizers = mvar : finalizers }

-- | Move a resource to the garbage collector, detaching it from its
-- original lifetime.
moveToGc :: Resource a -> IO (Cell a)
moveToGc r =
    mask $ \_ -> join $ atomically $ do
        value <- mustGetResource r
        fin <- detach r
        cell <- newCell value
        pure $ do
            addFinalizer cell fin
            pure cell
