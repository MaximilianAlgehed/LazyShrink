module LazyShrink where

import Data.IORef
import System.IO.Unsafe

{- It looks like instances of `Lazy` can be derived automatically with
 - very little effort -}
class Lazy a where
  numNodes :: a -> Int
  annotate :: Int -> IORef [Int] -> a -> (a, Int)
  prune    :: a -> [Int] -> a

ann :: IORef [Int] -> Int -> a -> a
ann ref i a = unsafePerformIO $ atomicModifyIORef ref (\is -> ((i:is), ())) >> return a

findIndecies :: Lazy a => a -> (a -> Bool) -> IO [Int]
findIndecies a pred = do
  ref <- newIORef []
  let (a', _) = annotate 0 ref a
  -- Evaluate as much as necessary
  if pred a' then return () else return ()
  readIORef ref

testAndPrune :: Lazy a => a -> (a -> Bool) -> IO a
testAndPrune t p = do
  idxs <- findIndecies t p
  return $ prune t idxs

{- Test predicate -}
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving Show

data Nat = Zero
         | Succ Nat
         deriving Show

instance Lazy a => Lazy (Tree a) where
  numNodes t = case t of
    Leaf       -> 1
    Node v l r -> 1 + numNodes v + numNodes l + numNodes r

  annotate i ref Leaf         = (ann ref i Leaf, i + 1)
  annotate i ref (Node v l r) =
    let (v', i')   = annotate (i + 1) ref v
        (l', i'')  = annotate i' ref l
        (r', i''') = annotate i'' ref r
    in (ann ref i (Node v' l' r'), i''')

  prune Leaf xs = Leaf
  prune (Node v l r) xs
    | 0 `notElem` xs = Leaf
    | otherwise      =
      let v' = prune v (map (\c -> c - 1) xs)
          l' = prune l (map (\c -> c - (numNodes v + 1)) xs)
          r' = prune r (map (\c -> c - (numNodes v + numNodes l + 1)) xs)
      in Node v' l' r'

instance Lazy Nat where
  numNodes t = case t of
    Zero   -> 1
    Succ n -> 1 + numNodes n

  annotate i ref Zero = (ann ref i Zero, i + 1)
  annotate i ref (Succ n) = 
    let (n', i') = annotate (i + 1) ref n
    in (ann ref i (Succ n'), i')

  prune Zero _ = Zero
  prune (Succ n) xs
    | 0 `notElem` xs = Zero
    | otherwise = Succ $ prune n (map (\c -> c - 1) xs)

gt :: Nat -> Nat -> Bool
gt Zero _            = False
gt (Succ x) Zero     = True
gt (Succ x) (Succ y) = x `gt` y

max' :: Nat -> Nat -> Nat
max' a b = if gt b a then b else a

height :: Tree a -> Nat
height Leaf = Zero
height (Node _ l r) = Succ (max' (height l) (height r))

lowerThanOrEqualTo :: Nat -> Tree a -> Bool
lowerThanOrEqualTo h t = h `gt` height t

example :: IO (Tree Nat)
example = testAndPrune (Node (Succ (Succ Zero))
                             (Node (Succ Zero)
                                   (Node (Succ (Succ Zero))
                                         (Node (Succ (Succ (Succ Zero))) (Node Zero (Node Zero Leaf Leaf) Leaf) Leaf)
                                         Leaf)
                                   Leaf)
                             (Node Zero (Node (Succ (Succ Zero)) (Node Zero Leaf Leaf) Leaf) Leaf)
                       )
                       (lowerThanOrEqualTo (Succ (Succ Zero)))
