{-# LANGUAGE DeriveDataTypeable #-}

module  Chapter3 (Chapter3(..)) where
  import Prelude hiding (tail, head)
  import Data.Typeable
  import Data.List (reverse)
  import Control.Exception

  data Chapter3 = Chapter3
  instance Exception HeapException

  data HeapException = HeapEmpty
                     deriving (Show, Typeable)

  class Emptiable h where
    empty :: Ord a  => h a
    isEmpty :: Ord a => h a -> Bool

  class Insertable h where
    insert :: Ord a => a -> h a -> h a

  class (Emptiable s, Insertable s) => Set s where
    member :: Ord a => a -> s a -> Bool

  class (Emptiable h, Insertable h) => Heap h where
    merge :: Ord a => h a -> h a -> h a
    findMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a

  data LeftistHeap a = E 
                     | LeftistHeap Int a (LeftistHeap a) (LeftistHeap a)
                     deriving (Show)
    
  instance Emptiable LeftistHeap where
    empty = E
    isEmpty E = True
    isEmpty _ = False

  rank E = 0
  rank (LeftistHeap r _ _ _) = r
  makeT x a b
    | rank a > rank b = LeftistHeap (rank b + 1) x a b
    | otherwise = LeftistHeap (rank a + 1) x b a
  
  instance Heap LeftistHeap where
    merge h1 E = h1
    merge E h2 = h2
    merge h1@(LeftistHeap _ x a1 b1) h2@(LeftistHeap _ y a2 b2)
      | x <= y = makeT x a1 (merge b1 h2)
      | otherwise = makeT y a2 (merge h1 b2)
    findMin E = throw HeapEmpty
    findMin (LeftistHeap _ x h1 h2) = x
    deleteMin (LeftistHeap _ x h1 h2) = merge h1 h2
    deleteMin _ = E

  instance Insertable LeftistHeap where
    insert x h = merge (LeftistHeap 1 x E E) h


  data T a = T Int a [T a]

  newtype BinomialHeap a = BinomialHeap [T a]
  
  instance Emptiable BinomialHeap where
    empty = BinomialHeap []
    isEmpty (BinomialHeap []) = True
    isEmpty _ = False

  rank' (T r x c) = r
  root' (T r x c) = x
  
  link t1@(T r1 x1 c1) t2@(T _ x2 c2) 
    | x1 <= x2 = T (r1 + 1) x1 (t2:c1)
    | otherwise = T (r1 + 1) x2 (t1:c2)

  insTree t [] = [t]
  insTree t ts@(t':ts') 
    | rank' t < rank' t' =  t:ts
    | otherwise = insTree (link t t') ts'
  
  instance Insertable BinomialHeap where
    insert x (BinomialHeap ts) = BinomialHeap (insTree (T 0 x []) ts)

  merge' ts1 [] = ts1
  merge' [] ts2 = ts2
  merge' ts1@(t1:ts1') ts2@(t2:ts2') =
          if rank' t1 < rank' t2 then t1:(merge' ts1' ts2)
          else if rank' t2 < rank' t1 then t2:(merge' ts1 ts2')
          else insTree (link t1 t2) (merge' ts1' ts2')

  removeMinTree [] = throw HeapEmpty
  removeMinTree [t] = (t, [])
  removeMinTree (t:ts)
    | root' t <= root' t' = (t, ts)
    | otherwise = (t', t:ts')
    where 
       (t', ts') = removeMinTree ts

  instance Heap BinomialHeap where
    merge (BinomialHeap ts1) (BinomialHeap ts2) = BinomialHeap (merge' ts1 ts2)
    findMin (BinomialHeap ts) = root' t
      where (t, _) = removeMinTree ts
    deleteMin (BinomialHeap ts) = BinomialHeap (merge' (reverse ts1) ts2)
      where (T _ x ts1, ts2) = removeMinTree ts


  data Color = R | B
  data RB a = E'
            | RB Color (RB a) a (RB a)

  instance Emptiable RB where
    empty = E'
    isEmpty E' = True
    isEmpty _ = False
  
  balance t = case t of 
    (RB B (RB R (RB R a x b) y c) z d) -> balanced a x b y c z d
    (RB B (RB R a x (RB R b y c)) z d) -> balanced a x b y c z d
    (RB B a x (RB R (RB R b y c) z d)) -> balanced a x b y c z d
    (RB B a x (RB R b y (RB R c z d))) -> balanced a x b y c z d
    (RB a b c d) -> (RB a b c d)
    where 
      balanced a x b y c z d = RB R (RB B a x b) y (RB B c z d)

  instance Insertable RB where
    insert x t = 
      let ins t = case t of
            E' -> RB R E' x E'
            (RB color a y b) -> if x < y then balance (RB color (ins a) y b)
                                else if y < x then balance (RB color a y (ins b))
                                else t
      in
         case ins t of
          (RB _ a y b) -> RB B a y b
          _ -> error "Impossible pattern for insert"

  instance Set RB where
    member x E' = False
    member x (RB _ a y b)
      | x < y = member x a
      | y < x = member x b
      | otherwise = True