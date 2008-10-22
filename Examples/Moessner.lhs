> import Prelude hiding (head, drop, repeat)
> import Stream
> import qualified Data.List as List 

> import Control.Applicative
> import Control.Monad.State.Lazy
> import Data.Traversable

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

Eric's attempt at Moessner:

Delete every n-th element.

-- ?remove :: State (Integer, (Stream (Maybe Integer)))

> remove :: Integer -> Stream Integer -> M (State Integer) (Stream (Maybe Integer))
> remove nth = disperse (WrapMonad step) (sift nth)

Stream elements are numbered starting from 1.

> step :: State Integer Integer
> step = do{ i <- get; put (i+1); return i}

> sift :: Integer -> Integer -> Integer -> Maybe Integer
> sift n k i = if i `mod` n == 0 then Nothing else Just k

Replace remaining elements by partial sums.

> partial_sum :: Stream (Maybe Integer) -> M(State Integer)(Stream (Maybe Integer))
> partial_sum = traverse step'

> step' :: Maybe Integer ->  M (State Integer) (Maybe Integer)
> step' Nothing = WrapMonad $ return Nothing
> step' (Just k) = WrapMonad $ do { i<- get; put (i+k); return (Just (i+k))}

> type M m a = WrappedMonad m a

disperse is a traversal that modifies elements effectfully but dependent on the state,
evolving the state independently of the elements.

> disperse :: (Traversable t, Applicative m) => m b -> (a -> b -> c) -> t a -> m (t c)
> disperse mb g = traverse (\a -> pure (g a) <*> mb)