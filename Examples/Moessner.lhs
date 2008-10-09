> import Prelude hiding (head, drop, repeat)
> import Stream
> import qualified Data.List as List 

Ralf's moessner definition:

> moessner :: (Num a) => [Stream a] -> Stream a
> moessner [s] = s
> moessner x = moessner [ s + psum (sum x') | s <- (lsum' x')]
>  where x' = init x

> rep :: Integer -> Stream a -> [Stream a]
> rep 0 s = []
> rep n s = s: rep (n-1) s


TMC's pointfree definition of moessner:

> moessner' k = List.head . app (k-1) (runSum . crossOut) . rep k
> 	where crossOut = init
> 	      runSum l = let k = toInteger $ length l
>	                 in  deleave k . psum' . interleave' k $ l

Successive function application:

> app :: Integer -> (a -> a) -> a -> a
> app 0 _ x = x
> app n f x = app (n-1) f (f x)

The next function is more efficient than app:

> app' k = (foldr (.) id) . (replicate k)

interleave' doesn't really need the argument. In fact, I can replace interleave' by
interleave, but I want to keep the symmetry.

> interleave' _ = interleave

Function |deleave k| is the inverse of |interleave' k|

> deleave k s = [ (cross k . drop d) s | d <- [0..(k-1)]]
>   where cross k s = Cons (head s) (cross k (drop k s))

Hinze's last version of moessner:

> moessner2 [s] = repeat s
> moessner2 x = moessner2 (List.tail (lrsum x')) +  s*(psum (moessner2 (List.replicate n 1)))
>  where x' = init x
>        s  = fromInteger $ List.sum x'
>        n = length x'
>        lrsum = scanl (+) 0

