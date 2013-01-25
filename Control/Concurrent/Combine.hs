module Control.Concurrent.Combine where

import Control.Concurrent.Combine.Transformer 

import Control.Concurrent.Async
import Control.Monad ( forM )

andthen s t = transform
    ( inside s `bind` inside t )
    ( outside t `bind` outside s )

orelse s t = transform 
    ( \ x -> do
        my <- inside s x
        case my of
            Just y -> return $ Just $ Left y
            Nothing -> efmap Right (inside t) x
    ) ( \ e -> case e of
        Left y -> outside s y
        Right z -> outside t z
    )

parallel ts = transform
    ( \ x -> runAnyJust $ map ( \ t -> inside t x ) ts)
    ( \ (k,y) -> outside (ts !! k) y )

runAnyJust actions = do
    as <- forM (zip [0 :: Int .. ] actions) 
        $ \ (k,action) -> 
           async $ do r <- action ; return (k, r)
    let waiter 0 = return Nothing
        waiter n = do
            (_ , (k,r)) <- waitAny as
            case r of
                Nothing -> waiter $ n - 1
                Just x  -> do
                    forM as cancel
                    return $ Just (k,x)
    waiter $ length actions 

        