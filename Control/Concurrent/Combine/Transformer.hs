module Control.Concurrent.Combine.Transformer where

import Control.Concurrent.Combine.Action (Action)
import qualified Control.Concurrent.Combine.Action as A

type Continuation out a = a -> Action out

newtype CPS out a = 
    CPS { unCPS :: Continuation out a -> Action out }

type Transformer a b c = a -> CPS c b

run :: CPS b b -> IO (Maybe b)
run c = A.run $ unCPS c return

instance Monad (CPS out) where
    fail msg = CPS $ \ k -> fail msg
    return x = CPS $ \ k -> k x
    CPS c >>= f = CPS $ \ k -> 
           c $ \ x -> unCPS (f x) $ k

orelse :: CPS out a -> CPS out a -> CPS out a   
orelse a b = CPS $ \ k -> 
    ( A.orelse (unCPS a k) (unCPS b k) ) 

parallel :: [CPS out a] -> CPS out a
parallel cs = CPS $ \ k ->
    ( A.parallel $ map (\ c -> unCPS c k) cs)

