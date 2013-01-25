{-# language ExistentialQuantification #-}

module Control.Concurrent.Combine.Transformer where

import Control.Monad ( guard, when )

type Action a b = a -> IO (Maybe b) 

bind f g = \ x -> do
    my <- f x
    case my of
        Nothing -> return Nothing
        Just y -> g y

efmap f a = \ x -> fmap (fmap f) (a x)

lift f = return . return . f

data Transformer a a' b' b = 
    Transformer { inside :: Action a a'
                , outside :: Action b' b 
                }

transform i o = 
    Transformer { inside = i, outside = o }

run :: Transformer a1 b' b' a -> a1 -> IO (Maybe a)
run t = inside t `bind` outside t 

assert :: (a' -> Bool) -> Transformer a' a' b b
assert pred = transform
    ( \ x -> return $ guard (pred x) >> return x ) 
    ( lift id )

fail = assert (const False)
