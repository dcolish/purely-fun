{-# LANGUAGE DeriveDataTypeable #-}

module Chapter2 where
  import Prelude hiding (tail, head)
  import Data.Typeable
  import Control.Exception

  instance Exception ListException

  class Emptiable s where
    empty :: Ord a => s a

  class (Emptiable st) => Stack st where
    isEmpty :: Ord a => st a -> Bool
    cons :: Ord a => a -> st a -> st a
    head :: Ord a => st a -> a
    tail :: Ord a => st a -> st a
  
  class (Emptiable st) => Set st where
    insert :: Ord a => a -> st a -> st a
    member :: Ord a => a -> st a -> Bool


  data ListException = Empty
                     deriving (Show, Typeable)

  data List a = Nil
              | Cons a (List a)
              deriving (Show)

  instance Emptiable List where
    empty = Nil
  
  instance Stack List where
    isEmpty Nil = True  
    isEmpty _ = False
    cons s l = Cons s (l)
    head (Cons s _) = s
    head Nil = throw Empty
    tail (Cons _ l) = l
    tail Nil = throw Empty


  data CustomStack a = None
                     | Push a (CustomStack a)
                     deriving (Show)

  instance Emptiable CustomStack where
    empty = None
    
  instance Stack CustomStack where
    isEmpty None = True
    isEmpty _ = False
    cons x s = Push x s
    head s = case s of
      None -> throw Empty
      Push a s' -> a
    tail s = case s of
      None -> throw Empty
      Push h s' -> s'


  data UnbalancedSet a = E
                       | UnbalancedSet (UnbalancedSet a) a (UnbalancedSet a)
                       deriving (Show)

  instance Emptiable UnbalancedSet where
    empty = E

  instance Set UnbalancedSet where
    member _ E = False
    member x (UnbalancedSet a y b)
      | x < y = member x a
      | y < x = member x b
      | otherwise = True

    insert x E = UnbalancedSet E x E
    insert x (UnbalancedSet a y b) 
      | x < y = UnbalancedSet (insert x a) y b
      | y < x = UnbalancedSet a y (insert x b)
      | otherwise = (UnbalancedSet a y b)
