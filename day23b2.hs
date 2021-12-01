import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as V

import Data.Char
import Control.Monad

-- main = interact $ f' . map digitToInt . head
main = do
    print $ f' [1,5,6,7,9,4,8,2,3]  -- (84.99 secs, 27,793,109,552 bytes)

nIters = 10000000
n = 1000000
dec 0 = n - 1
dec x = x - 1

h v current = do
    x1 <- V.read v current  -- Follow the next pointers (like a singly-linked list) into and through the trio.
    x2 <- V.read v x1
    x3 <- V.read v x2
    next <- V.read v x3 -- Get the number that the end of the trio points to.
    let dec' x = if x == x1 || x == x2 || x == x3 then dec' $ dec x else x
        x = dec' $ dec current
    V.read v x >>= V.write v x3  -- Copy the 'next' of new value with the 'next' of last of trio to be 'moved', effectively moving the end of trio. (>>=) Runs one command after the other, passing results, but not storing it.
    V.write v x x1  -- Replace the 'next' of the new decremented value to point to front of trio.
    V.write v current next  -- Current (used to point to start of trio) now should point to the next value.
    return next  -- Do everything again with the new pointer.

f' xs = runST $ do
    v <- V.new n
    zipWithM_ (V.write v) xs' (tail $ cycle xs') -- ? Writes (V.Write v), using (xs') as position and (tail $ cycle xs') as value. Thus, xs' becomes a list of 'next' pointers. Prevents shifting and growing of lists.
    foldM_ (const . h v) (head xs') [1..nIters]  -- ? The const throws away the new value (just counting 1..nIters]) and only uses the accumulator, passing it to (h v) each time.
    r1 <- V.read v 0
    r2 <- V.read v r1
    return $ (r1+1) * (r2+1)  -- ? Adding one back, since it was subtracted in the where clause at the beginning, when the source list was created.
    where
        xs' = map (+ (-1)) xs ++ [length xs .. n - 1]  -- ? Creates a list of values with given prefix list (minus 1, since it has 1 as lowest) and then adds the rest, to the limit.