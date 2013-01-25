module Control.Concurrent.Combine.Transformer where

import Control.Concurrent.Combine.Action (Action)
import qualified Control.Concurrent.Combine.Action as A

type Computer a b = a -> Action b

andthen :: Computer a b -> Computer b c 
        -> Computer a c
andthen f g = \ x -> f x >>= g

orelse :: Computer a b -> Computer a b
       -> Computer a b
orelse f g = \ x -> A.orelse (f x) (g x)

parallel :: [ Computer a b ] -> Computer a b
parallel fs = 
   \ x -> A.parallel $ map ( \ f -> f x ) fs

sequential :: [ Computer a b ] -> Computer a b
sequential fs = \ x -> foldr A.orelse (fail "seq") 
           $ map ( \ f -> f x ) fs

