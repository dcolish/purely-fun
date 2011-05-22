exception Empty
exception Impossible_pattern of string

module type ORDERED = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end
    
module type SET = sig
  type elem
  type set

  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end

module type Heap = sig
  type elem
  type heap

  val empty : heap
  val isEmpty : heap -> bool
  val insert : elem -> heap -> heap
  val merge : heap -> heap -> heap
  val findMin : heap -> elem
  val deleteMin : heap -> heap
end

 
module LeftistHeap (Element : ORDERED) : (Heap with type elem = Element.t) =
struct
  type elem = Element.t
  type tree = E | T of int * elem * tree * tree
  type heap = tree

  let empty = E
  let isEmpty = function
    | E -> true 
    | _ -> false
  let rank = function
    | E -> 0
    | T(r, _, _, _) -> r

  let makeT x a b = 
    if rank a > rank b then T(rank b + 1, x, a, b)
    else T(rank a + 1, x, b, a)
     
  let rec merge h1 h2 = 
    match h1, h2 with
      | _, E -> h1
      | E, _ -> h2
      | T(_, x, a1, b1), T(_, y, a2, b2) -> 
          if x <= y then makeT x a1 (merge b1 h2)
          else makeT y a2 (merge h1 b2)

  let insert x h = merge (T(1, x, E, E)) h
  let findMin = function
    | T(_, x, _, _) -> x
    | E -> raise Empty
  let deleteMin = function
    | T(_, x, a, b) -> merge a b
    | _ -> E
end

module BinomialHeap (Element : ORDERED) : (Heap with type elem = Element.t) = 
struct
  type elem = Element.t
  type tree = T of int * elem * tree list
  type heap = tree list

  let empty = []
  let isEmpty = function
    | [] -> false 
    | _ -> true
  let rank (T(r, x, c)) = r
  let root (T(r, x, c)) = x
  let link  (T(r1, x1, c1) as t1) (T(_, x2, c2) as t2) =
      if Element.leq x1 x2 then T(r1 + 1, x1, t2::c1)
      else T(r1 + 1, x2, t1::c2)

  let rec insTree t = function
    | [] -> [t]
    | t'::ts' as ts -> if rank t < rank t' then t :: ts
      else insTree (link t t') ts'

  let insert x ts = insTree (T(0, x, [])) ts

  let rec merge ts1 ts2 = 
    match ts1, ts2 with
      | _, [] -> ts1
      | [], _ -> ts2
      | t1 :: ts1', t2 :: ts2' -> 
          if rank t1 < rank t2 then t1::merge ts1' ts2
          else if rank t2 < rank t1 then t2::merge ts1 ts2'
          else insTree (link t1 t2) (merge ts1' ts2')

  let rec removeMinTree = function
    | [] -> raise Empty
    | [t] -> (t, [])
    | (t::ts) -> let (t', ts') = removeMinTree ts
      in if Element.leq (root t) (root t') then (t, ts) else (t', t::ts')

  let findMin ts = let (t, _) = removeMinTree ts in root t
  let deleteMin ts = let (T(_, x, ts1), ts2) = removeMinTree ts in
    merge (List.rev ts1) ts2
end

                                                      
module RedBlackSet (Element : ORDERED) : (SET with type elem = Element.t) = 
struct
  type elem = Element.t
  type color = R | B
  type tree = E | T of color * tree * elem * tree
  type set = tree

  let empty = E

  let rec member x = function
    | E -> false
    | T(_, a, y, b) ->
        if Element.lt x y then member x a
        else if Element.lt y x then member x b
        else true

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d
    | B, a, x, T (R, T (R, b, y, c), z, d)
    | B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | a, b, c, d -> T (a, b, c, d)

  let insert x s =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s ->
          if Element.lt x y then balance (color, ins a, y, b)
          else if Element.lt y x then balance (color, a, y, ins b)
          else s
    in
      match ins s with
        | T(_, a, y, b) -> T (B, a, y, b)
        | _ -> raise (Impossible_pattern "insert")
end
