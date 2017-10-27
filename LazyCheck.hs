module LazyCheck where

import Test.QuickCheck

import LazyShrink

class Mutate a where
  mutate :: a -> Gen a

lazyCheck :: (Arbitrary a, Mutate a, Lazy a) => (a -> Bool) -> IO (Maybe a)
lazyCheck p = generate arbitrary >>= go 100
  where
    go 0 a = return Nothing
    go n a
      | p a       = do
        a' <- testAndPrune a p 
        generate (mutate a) >>= go (n - 1)
      -- a is a counterexample
      | otherwise = return (Just a)

{- Example -}
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where 
      go 0 = return Leaf
      go d = oneof [ return Leaf, Node <$> arbitrary <*> go (d - 1) <*> go (d - 1) ]

{- I don't like this implementation of `mutate`,
 - I want the semantics to be something like
 - "change an arbitrary, _evaluated_ node into something else" -}
instance (Arbitrary a, Mutate a) => Mutate (Tree a) where
  mutate Leaf         = Node <$> arbitrary <*> arbitrary <*> arbitrary
  mutate (Node v l r) = oneof [ Node <$> mutate v <*> return l <*> return r
                              , Node <$> return v <*> mutate l <*> return r
                              , Node <$> return v <*> return l <*> mutate r
                              , return Leaf ]

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n - 1))

instance Arbitrary Nat where
  arbitrary = int2Nat . abs <$> arbitrary

instance Mutate Nat where
  mutate Zero = Succ <$> arbitrary
  mutate (Succ n) = oneof [ return n
                          , Succ <$> mutate n
                          , return $ Succ (Succ n)
                          , return Zero ]
