module LazyShrink where

import Data.IORef
import System.IO.Unsafe

data Tree = Node Int Tree Tree
          | Leaf
          deriving (Ord, Eq, Show)

numNodes :: Tree -> Int
numNodes Leaf = 1
numNodes (Node _ l r) = 1 + numNodes l + numNodes r

annotate :: Int -> IORef [Int] -> Tree -> (Tree, Int)
annotate i ref Leaf         = (unsafePerformIO $ atomicModifyIORef ref (\is -> ((i:is), ())) >> return Leaf, i + 1)
annotate i ref (Node k l r) =
  let (l', i')  = annotate (i + 1) ref l
      (r', i'') = annotate i' ref r
  in (unsafePerformIO $ atomicModifyIORef ref (\is -> ((i:is), ())) >> return (Node k l' r'), i'' + 1)

findIndecies :: Tree -> (Tree -> Bool) -> IO [Int]
findIndecies tree pred = do
  ref <- newIORef []
  let (tree', _) = annotate 0 ref tree
  -- To make sure that (pred tree') is evaluated
  if (pred tree') then return () else return ()
  readIORef ref

pruneAllExcept :: Tree -> [Int] -> Maybe Tree
pruneAllExcept Leaf xs = if 0 `notElem` xs then Nothing else Just Leaf
pruneAllExcept (Node k l r) xs
  | 0 `notElem` xs = Nothing
  | otherwise      =
    let l' = pruneAllExcept l (map (\c -> c - 1) xs)
        r' = pruneAllExcept r (map (\c -> c - (numNodes l + 1)) xs)
    in Just $ Node k (maybe Leaf id l') (maybe Leaf id r')

testAndPrune :: Tree -> (Tree -> Bool) -> IO Tree
testAndPrune t p = do
  idxs <- findIndecies t p
  return $ maybe Leaf id (pruneAllExcept t idxs)

{- Test predicate -}
data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

gt :: Nat -> Nat -> Bool
gt Zero _            = False
gt (Succ x) Zero     = True
gt (Succ x) (Succ y) = x `gt` y

max' :: Nat -> Nat -> Nat
max' a b = if gt a b then a else b

height :: Tree -> Nat
height Leaf = Zero
height (Node _ l r) = Succ (max' (height l) (height r))

higherThan :: Nat -> Tree -> Bool
higherThan h t = h `gt` (height t)

example :: IO Tree
example = testAndPrune (Node 0
                             (Node 1
                                   (Node 2
                                         (Node 3 Leaf Leaf)
                                         Leaf)
                                   Leaf)
                             (Node 0 Leaf Leaf)
                       )
                       (higherThan (Succ (Succ Zero)))
