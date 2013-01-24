module Control.Concurrent.Combine where

import Control.Concurrent.Combine.Transformer 

import Control.Monad ( guard )

assert :: (a1 -> Bool) -> Transformer a1 a1 b b
assert pred = transform
    ( \ x -> do guard $ pred x ; return x ) id

