module Control.Concurrent.Combine.Action where

newtype Action a b = 
    Action ( a -> IO (Maybe b) )

action :: (a -> IO (Maybe b)) 
       -> Action a b
action f = Action f

execute :: Action a b -> a -> IO (Maybe b)
execute (Action a) = a

fail = action $ \ a -> return Nothing

succeed f = action $ \ x -> return $ Just $ f x

