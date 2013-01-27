module Control.Concurrent.Combine.Action where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad ( MonadPlus (..), forM)
import Control.Exception ( throwIO )

import System.IO

-- | An @Action@ is a process that runs for some time,
-- and then returns a result, which may be
-- negative (@Nothing@) or positive (@Just@).
-- As an application, think of running a SAT solver,
-- and the result could be "unsatisfiable" (@Nothing@)
-- or "satisfiable" with a satisfying assignment
-- (@Just model@).
-- @Action@s can also be run concurrently,
-- and could possibly be threadKilled.

data Action b = Action { run :: IO (Maybe b) }

io :: IO (Maybe b) -> Action b
io = Action

lift :: Action b -> Action (Maybe b)
lift a = Action $ do x <- run a ; return $ Just x

instance Monad Action where
    return x = Action $ return $ return x
    fail msg = Action $ return Nothing
    Action a >>= f = Action $ do
        mx <- a
        case mx of 
            Nothing -> return Nothing
            Just x -> run $ f x 

instance MonadPlus Action where
    mzero = Action $ return Nothing
    mplus = orelse

-- | @orelse a b@ runs @a@ first. 
-- If @a@ finishes successfully, 
-- then this gives the result.
-- If @a@ produces Nothing, then @b@ is started,
-- and gives the result.
-- (@orelse@ is also @mplus@ 
-- of the @MonadPlus@ instance)
orelse :: Action b -> Action b -> Action b
orelse a b = do
        my <- lift $ a
        case my of
            Just y -> return y
            Nothing -> b

-- | @parallel xs@ starts all actions concurrently.
-- When the first non-@Nothing@ result is obtained,
-- all other actions are killed, and the result
-- is @Just@.
-- When all finish with @Nothing@, 
-- then the result is @Nothing@.
parallel :: [ Action b ] -> Action b
parallel actions = Action $ do
    as <- forM actions $ \ act -> async $ run act
    out <- atomically $ do
        ps <- forM as pollSTM
        let good = do
               Just (Right (Just x)) <- ps
               return x
            running = do 
               Nothing <- ps ; return ()
            bad = do 
               Just (Left e) <- ps
               return e
        case bad of 
            e : _ -> return $ Left e 
            [] -> case good of
                x : _ -> return $ Right $ Just x 
                [] -> case running of
                    [] -> return $ Right Nothing
                    _ -> retry
    -- hPutStrLn stderr "*********** cancel"
    forM as cancel
    case out of
        Right x -> return x
        Left  e -> throwIO e



