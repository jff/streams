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
> import Data.Traversable
> import Data.Foldable hiding (sum, foldr)

> data Stream a  = Cons a (Stream a) 

For Eq and Show instances, we only consider the first |ntoshow| elements of the stream.

> ntoshow = 30

Instances: 

First, we want to manipulate streams of numbers as if they were numbers.

> instance (Num a) => Num (Stream a) where
>	(+) = zip (+)
>	(*) = zip (*)
>	negate = map negate
>	fromInteger i = repeat (fromInteger i)
>	abs = map abs
>	signum = map signum

We show the first |ntoshow| elements using a list notation.

> instance (Show a) => Show (Stream a) where
>	show s = "[" ++ limited_show ntoshow s
>	 where  limited_show 1 (Cons h t)          = show h ++ "]"
>       	limited_show n (Cons h t) = show h ++ "," ++ limited_show (n-1) t

And we want to compare the first |ntoshow| elements of two streams.

> instance (Eq a) => Eq (Stream a) where
>	s == t = eqStream ntoshow s t
>	  where eqStream 0 _ _ = True
>		eqStream k (Cons a t) (Cons a' t') = (a==a') && eqStream (k-1) t t' 



Now, we define some useful functions on streams.

> head :: Stream a -> a
> head (Cons a t) = a

> tail :: Stream a -> Stream a
> tail (Cons a t) = t

> repeat :: a -> Stream a
> repeat a = Cons a (repeat a)

> map :: (a -> b) -> Stream a -> Stream b
> --map f s = Cons (f (head s)) (map f (tail s))
> map = fmap

> zip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
> zip f s t = Cons (f (head s) (head t)) (zip f (tail s) (tail t))

> interleave :: [Stream a] -> Stream a
> interleave (s:x) = Cons (head s) (interleave (x ++ [tail s]))

Functions take and drop may be useful:

> take :: Integer -> Stream a -> [a]
> take 0 _ = []
> take n (Cons a t) = a:(take (n-1) t)

> drop :: Integer -> Stream a -> Stream a
> drop 0 s = s
> drop n (Cons a t) = drop (n-1) t




The following functions are useful for dealing with streams of numbers.

> psum :: (Num a) => Stream a -> Stream a
> psum s = t where t = Cons 0 (t+s)

> psum' :: (Num a) => Stream a -> Stream a        
> psum' = tail . psum

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
>   fmap f = (pure f <*>)

Traversal involves iterating over the elements of a stream
, in the style of a `map', but interpreting certain function
applications idiomatically. Note the similarity to |fmap|.

> instance Traversable Stream where
>   traverse f (Cons x xs) = pure Cons <*> f x <*> traverse f xs

> instance Foldable Stream where
>   foldMap = foldMapDefault

