module Control.Concurrent.Combine.Transformer where

import Control.Concurrent.Combine.Action (Action)
import qualified Control.Concurrent.Combine.Action as A

newtype Transformer a a' b' b =
    Transformer ( Action a' b' -> Action a b )

fail = Transformer $  \ _ -> A.fail

apply :: Transformer a a' b' b
      -> Action a' b'
      -> Action a b
apply (Transformer t) a = t a

run :: Transformer a b' b' b -> a -> IO (Maybe b)
run t = A.execute (apply t $ A.succeed id)

transform fore back = Transformer $ \ act -> 
    A.action $ \ input -> case fore input of
        Just input' -> fmap back $ A.execute act input'
        Nothing     -> return Nothing
