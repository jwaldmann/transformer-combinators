module Control.Concurrent.Combine.Lifter 

( module Control.Concurrent.Combine.Lifter 
, module Control.Concurrent.Combine.Computer
)

where

import Control.Concurrent.Combine.Computer 

-- | A @Lifter@ will produce a Computer
-- that accepts a continuation and produces a result.
-- A @Lifter@ is a @Computer@ so you can apply
-- combinators of @Computer@s, especially @parallel@

type Lifter a b c = 
     Computer a (Computer (Computer b c) c)

-- | consider these applications:
-- @p = apply (parallel xs) y@
-- and @q = parallel (map (\x -> apply x y) xs)@.
-- The parallelism in @p@ is restricted:
-- the first successful x in xs is determined,
-- and then combined with y.
-- In @q@, each x of xs is combined with y,
-- and the winner is determined globally.
apply :: Lifter a b c 
      -> Computer b c
      -> Computer a c
apply f k = \ sys -> do m <- f sys ; m k

