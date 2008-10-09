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
> import Control.Applicative
> --import Data.Traversable
> --import Data.Foldable hiding (sum, foldr)

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

map f s = Cons (f (head s)) (tail s)

> map f s = Cons (f (head s)) (map f (tail s))
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




The Stream functor induces a monadic applicative functor

> instance Applicative Stream where
>   pure x = xs where xs = Cons x xs --(repeat)
>   (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs) --(map)

> instance Functor Stream where
>   fmap f (Cons x xs) = Cons (f x) (fmap f xs)

Traversal involves iterating over the elements of a stream
, in the style of a `map', but interpreting certain function
applications idiomatically. Note the similarity to |fmap|.

> --instance Traversable Stream where
>   --traverse f (Cons x xs) = pure Cons <*> f x <*> traverse f xs

> --instance Foldable Stream where
>   --fold xs = mconcat (to_list xs)

