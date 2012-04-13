(* Chapter 4: Lazy Evaluation  *)

(* Streams *)
      
let force = Lazy.force

module type STREAM =
sig
  type 'a stream

  val (++) : 'a stream -> 'a stream -> 'a stream
  val take : int -> 'a stream -> 'a stream
  val drop : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
end

module Stream : STREAM =
struct
  type 'a stream = Nil | Cons of 'a * 'a stream lazy_t

  let rec (++) s1 s2 = match s1 with
    | Nil -> s2
    | Cons(hd, tl) -> Cons(hd, lazy (force tl ++ s2))

  let rec take x = function
    | _ when x = 0 -> Nil
    | Nil -> Nil
    | Cons(hd, tl) -> Cons(hd, lazy (take (x - 1) (force tl)))

  let rec drop x = function
    | s when x = 0 -> s
    | Nil -> Nil
    | Cons(hd, tl) -> drop (x - 1) (force tl)

  let reverse s = 
    let rec reverse' s1 = function
      | Nil -> s1
      | Cons(hd, tl) -> reverse' (Cons (hd, lazy s1)) (force tl) in
      reverse' Nil s
end

