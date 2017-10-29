module LazyCheck where

import Test.QuickCheck

import LazyShrink

class Mutate a where
  mutate :: a -> Gen a

lazyCheck :: (Arbitrary a, Mutate a, Lazy a) => (a -> Bool) -> IO (Maybe a)
lazyCheck p = generate arbitrary >>= go 1000 >>= maybe (return Nothing) (fmap Just . lazyShrink p)
  where
    go 0 a = return Nothing
    go n a
      | p a       = do
        a' <- testAndPrune a p 
        generate (mutate a) >>= go (n - 1)
      -- a is a counterexample
      | otherwise = return (Just a)

{- Example -}

{- I don't like this implementation of `mutate`,
 - I want the semantics to be something like
 - "change an arbitrary node into something else" -}
instance (Arbitrary a, Mutate a) => Mutate (Tree a) where
  mutate t = do
    let nodes = go t :: Int
    i <- choose (0, nodes)
    change i t
    where
      go Leaf = 1
      go (Node _ l r) = 1 + go l + go r
      
      change 0 t = arbitrary
      change n Leaf = return Leaf
      change n (Node v l r) =
        let n' = n - 1 in
        if n' > go l then
          Node v l <$> change (n' - go l) r
        else
          Node v <$> change n' l <*> return r 

instance Mutate Nat where
  mutate Zero = Succ <$> arbitrary
  mutate (Succ n) = oneof [ return n
                          , Succ <$> mutate n
                          , return $ Succ (Succ n)
                          , return Zero ]
