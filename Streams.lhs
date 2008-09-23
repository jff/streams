This module defines a datatype Stream and useful functions on Streams.

> module Stream where

> import Prelude hiding (
>                head,
>                tail,
>                repeat,
>                map,
>                zip,
>                take,
>                drop
>                )

> import qualified Data.List as List 

> data Stream a  = Cons a (Stream a) 

For Eq and Show instances, we only consider the first |ntoshow| elements of the stream.

> ntoshow = 30

Instances:

> instance (Num a) => Num (Stream a) where
>	(+) = zip (+)
>	(*) = zip (*)
>	negate = map negate
>	fromInteger i = repeat (fromInteger i)
>	abs = map abs
>	signum = map signum

> instance (Show a) => Show (Stream a) where
>	show s = "[" ++ limited_show ntoshow s
>	 where  limited_show 1 (Cons h t)          = show h ++ "]"
>       	limited_show n (Cons h t) = show h ++ "," ++ limited_show (n-1) t

> instance (Eq a) => Eq (Stream a) where
>	s == t = eqStream ntoshow s t
>	  where eqStream 0 _ _ = True
>		eqStream k (Cons a t) (Cons a' t') = (a==a') && eqStream (k-1) t t' 



We now define some useful functions on streams.

> head (Cons a t) = a
> tail (Cons a t) = t
> repeat a = Cons a (repeat a)
> map f s = Cons (f (head s)) (tail s)
> zip f s t = Cons (f (head s) (head t)) (zip f (tail s) (tail t))
> interleave (s:x) = Cons (head s) (interleave (x ++ [tail s]))

> psum s = t where t = Cons 0 (t+s)
> psum' = tail . psum

Functions take and drop may be useful:

> take :: Integer -> Stream a -> [a]
> take 0 _ = []
> take n (Cons a t) = a:(take (n-1) t)

> drop :: Integer -> Stream a -> Stream a
> drop 0 s = s
> drop n (Cons a t) = drop (n-1) t

Auxiliary functions on lists:

> lsum :: (Num a) => [Stream a] -> [Stream a]
> lsum = scanl (+) 0
> lsum' :: (Num a) => [Stream a] -> [Stream a]
> lsum' = List.tail . lsum

Some examples:

> ones = Cons 1 ones
> nat = Cons 0 (nat + 1)
> nat' = nat + 1

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

