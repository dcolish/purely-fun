(* Exercises from chapter 2 *)

exception Subscript

exception Empty

module type Stack = sig
  type 'a stack
  val empty : 'a stack
  val isEmpty : 'a stack -> bool
  val cons : 'a -> 'a stack -> 'a stack
  val head : 'a stack -> 'a
  val tail : 'a stack -> 'a stack
end

module ListStack : Stack = struct
  type 'a stack = 'a list
  let empty = []
  let isEmpty s = s = []
  let cons x s = x :: s
  let head s = match s with [] -> raise Empty | h::_ -> h
  let tail s = match s with [] -> raise Empty | _::t -> t
end

module CustomStack : Stack = struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let empty = Nil
  let isEmpty s = s = Nil
  let cons x s = Cons(x, s)
  let head = function Nil -> raise Empty | Cons(x,_) -> x
  let tail = function Nil -> raise Empty | Cons(_,s) -> s
end
    
module type SET = sig
  type elem
  type set
  val empty : set
  val insert : elem -> set -> set
  val member : elem -> set -> bool
end

module type ORDERED = sig
  type t
  val e : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end


module UnbalancedSet (Element : ORDERED) : (SET with type elem = Element.t) = 
struct
  type elem = Element.t
  type tree = E | T of tree * elem * tree
  type set = tree

  let empty = E

  let rec member x = function
    |  E -> false
    | T(a, y, b) when Element.lt x  y -> member x a
    | T(a, y, b) when Element.lt y x -> member x b
    | _ -> true

  let rec insert x = function
    | E -> T(E, x, E)
    | T(a, y, b) when Element.lt x y -> T(insert x a, y, b)
    | T(a, y, b) when Element.lt y x -> T(a, y, insert x b)
    | T(a, y, b) as s -> s
end

module type FiniteMap = sig
  type key
  type 'a map 

  val empty: 'a map
  val bind : key -> 'a -> 'a map -> 'a map
  val lookup : key -> 'a map -> 'a
end


let rec suffixes xs =
  match xs with
      x :: xs -> (x::xs) :: suffixes xs
    | [] -> [[]]
;;

suffixes [1;2;3;4];;


type 'a tree = Nothing
            | Tree of 'a tree * 'a * 'a tree;;

let build_tree =
  let t = insert 3 Nothing in
  let u = insert 2 t in
    insert 1 u;;

assert (List.fold_left (&) true (List.map (fun n -> member n build_tree) [1;2;3]));;
