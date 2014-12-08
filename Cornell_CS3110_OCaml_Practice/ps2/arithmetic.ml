type exp = Val of int | Plus of exp * exp | Times of exp * exp

let example_exp = 
				Times (
					Plus (Val 1 , Val 1),
					Plus (Times (Val 3 , Val 4),
						 Val 1)
				)

(* Write exp_fold that recursively processes each subexpression 
of exp and then combines their results using the following scheme 
-applies the operation val_op to each Val subexpression
-applies the operation plus_op to each Plus subexpession
-applies the operation times_op to each Times subexpression *)
let rec exp_fold val_op plus_op times_op exp = 
	let recCall = exp_fold val_op plus_op times_op in
match exp with 
| Val v -> val_op v
| Plus (x,y) -> plus_op (recCall x) (recCall y)
| Times (x,y) -> times_op (recCall x) (recCall y)

(* (a) Write a function eval : exp -> int that takes an input expression
 and evaluates it ac- cording to the rules of ordinary integer arithmetic *)

 let eval = exp_fold (fun x -> x) (fun x y -> x + y) (fun x y -> x * y)

 (* Write a function to_string : exp -> string that returns a fully 
 parenthesized string rep- resentation of the input expression,
  with no spaces between operators and their operands *)

let to_string = 
	let val_op = (fun x -> string_of_int x) in
	let plus_op = (fun x y -> Printf.sprintf "(%s + %s)" x y) in 
	let times_op = (fun x y -> Printf.sprintf "(%s * %s)" x y) in
exp_fold  val_op plus_op times_op