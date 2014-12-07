exception lookOver

(* Warm-Up *)
(* Exercise 1: Identify the types and values of following expressions. *)

(* a. 3 + 5  *)
(* int -> int -> int, value = 8 *)

(* b. ("zardoz", 3 + 5) *)
(* string * int *)

(* c. ["zardoz"; 3+5] *)
(* Ill-typed, lists must be of a single type *)

(* d. [List.hd] :: [] *)
(* ('a list -> 'a) list list) *)  raise lookOver

(* e. fun x y -> if x then y else x *)
(* bool -> bool -> bool *)

(* f. fun a (b,c) -> a c (b c) *)
(* ('a -> 'b -> 'c) -> 'a -> 'b * 'b -> 'c *)

(* Exercise 2: Give expression having the following types: *)

(* a. int -> int -> int
	fun a b -> a + b

b. (int -> int) -> int
	let exp a = (a 5) + 5
 *)
c. int -> (int -> int)

d. 'a -> 'a

e. 'a -> 'b -> 'a

f. ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
Take a function that takes a tuple and returns a 'a

