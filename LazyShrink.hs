module LazyShrink where

import Data.IORef
import System.IO.Unsafe
import Data.List

import Test.QuickCheck
import Test.LeanCheck
import Test.LazySmallCheck hiding (cons0, cons1, cons3, (\/))
import qualified Test.LazySmallCheck as L

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
  return $ prune t (sort idxs)

lazyShrink :: (Lazy a, Arbitrary a) => (a -> Bool) -> a -> IO a
lazyShrink p a = testAndPrune a p >>= (\a -> go (filter (not . p) $ shrink a) a)
  where
    go [] a = return a 
    go (a:as) a_old = do
      a' <- testAndPrune a p
      if p a' then
        go as a_old
      else
        go (filter (not . p) (shrink a') ++ filter (\a_s -> numNodes a_s < numNodes a') as) a

{- Test predicate -}
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving Show

data Nat = Zero
         | Succ Nat
         deriving Show

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where 
      go 0 = return Leaf
      go d = oneof [ return Leaf, Node <$> arbitrary <*> go (d - 1) <*> go (d - 1) ]

  shrink Leaf = []
  shrink (Node v l r) =  (Node <$> take 3 (shrink v) <*> return l <*> return r)
                      ++ [l]
                      ++ [r]
                      ++ take 1000 (Node <$> return v <*> shrink l <*> return r)
                      ++ take 1000 (Node <$> return v <*> return l <*> shrink r)

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

  prune (Node v l r) xs@(0:_) =
      let v' = prune v $ truncate 1 xs
          l' = prune l $ truncate (1 + numNodes v) xs
          r' = prune r $ truncate (1 + numNodes v + numNodes l) xs
          truncate i xs = filter (>=0) $ map (\x -> x - i) xs
      in Node v' l' r'
  prune _ _ = Leaf 

instance Listable a => Listable (Tree a) where
  tiers =  cons0 Leaf
        \/ cons3 Node

instance Serial a => Serial (Tree a) where
  series =  L.cons0 Leaf
         L.\/ L.cons3 Node

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n - 1))

instance Arbitrary Nat where
  arbitrary = int2Nat . abs <$> arbitrary

  shrink Zero = []
  shrink (Succ n) = n : shrink n

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

instance Listable Nat where
  tiers =  cons0 Zero
        \/ cons1 Succ

instance Serial Nat where
  series =  L.cons0 Zero
         L.\/ L.cons1 Succ

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

isBST :: Tree Nat -> Bool
isBST Leaf = True
isBST (Node n l r) = isBST l && isBST r && allAre (\v -> n `gt` v) l && allAre (\v -> v `gt` n) r
  where
    allAre p Leaf = True
    allAre p (Node v l r) = p v && allAre p l && allAre p r
