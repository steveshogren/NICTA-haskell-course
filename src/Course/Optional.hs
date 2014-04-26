{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import Course.Core
import qualified Prelude as P

--  class Optional<A> {
--    Optional(A a) {} // Full
--    Optional() {} // Empty
--  }
data Optional a = Full a | Empty deriving (Eq, Show)


-- map just unpacks a single value to apply the function if Full
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty    = Empty
mapOptional f (Full a) = Full (f a)

-- bind is the same as map, but with a function that returns
-- an Optional
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty    = Empty
bindOptional f (Full a) = f a

-- The same ?? from C#, only it takes an Optional 
-- not a Nullable<T>
(??) :: Optional a -> a -> a
Empty ?? d  = d
Full a ?? _ = a

-- Will return the second Optional if the first is
-- Empty, or the first if it is Full. Does no checks
-- on the second
(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _     = k

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f a b = bindOptional (\aa -> mapOptional (f aa) b) a

-- this "Eq a" appears to be anything a, as long as it implements
-- the Eq... interface?
contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
