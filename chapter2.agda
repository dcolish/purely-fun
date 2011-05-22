module chapter2 where
 
data Bool : Set where
  false : Bool
  true : Bool

_&&_ : Bool -> Bool -> Bool
true && true = true
true && false = false
false && true  = false
false && false = false

data Nat : Set where
  zero : Nat
  suc : Nat -> Nat

_==_ : Nat -> Nat -> Bool
zero == zero = true
zero == (suc _) = false
(suc _) == zero = false
(suc a) == (suc b) = (a == b)

_+_ : Nat -> Nat -> Nat
zero + zero = zero
(suc a) + zero = suc a
zero + (suc a) = suc a
(suc a) + (suc b) = suc (suc (a + b))


data List (A : Nat) :  Nat -> Set where
  []   : List A zero
  _::_ : Nat -> {n : Nat} -> List A n -> List A (suc n)

empty : {n : Nat} -> List n zero
empty = []

isEmpty : {m n : Nat} -> List n m -> Bool
isEmpty [] = true
isEmpty _ = false

cons : {m n : Nat} -> Nat -> List n m -> List n (suc m)
cons a l = a :: l

head : {m n : Nat} -> List m (suc n) -> Nat
head (x :: xs) = x

tail : {m n : Nat} -> List m (suc n) -> List m n
tail (x :: xs) = xs

-- _listeq_ : {m n : Nat} -> List m n -> List m n -> Bool
-- [] listeq [] = true
-- (x :: xs) listeq (y :: ys) = (x == y) && (xs listeq ys)

rev : {m n : Nat