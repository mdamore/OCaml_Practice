(* Exercise 1: *)

(* Write a function sum (lst : int list) : int that sums up the elements of lst *)

let sum l = List.fold_left (+) 0 l

(* Write a function rev (lst : 'a list) : 'a list that reverses lst. *)

let rev lst = List.fold_left (fun acc x -> x::acc) [] lst in rev [1;2;3;4;5]

(* Write a function max2 (lst : 'a list ) : int that returns the second greatest unique element in lst, 
or fails if the list contains fewer than two distinct elements. *)

let max2 lst = let f l x = match l with
	| [] -> [x]
	| hd::[] -> if (hd = x) then l else if (x > hd) then x::l else [hd;x]
	| hd::xs::tl -> if ((hd = x) || (xs = x)) then l
				else if (x > hd) then ([x;hd]) 
 				else if (x > xs) then ([hd;x]) 
 				else l

in
match List.fold_left f [] lst with
| [] -> raise (Failure "max2: Fewer than two distinct elements")
| hd::[] -> raise (Failure "max2: Fewer than two distinct elements")
| _::return::tl -> return


(* Exercise 2: Write each function recursively, using fold, and using List module w/o rec *)

(* Write a function lengths (lsts : 'a list list) : int list
that computes an int list containing the length of each element of lsts. 
You may use List.length in all three strategies for this problem *)

let lengths_rec lsts = 
	let rec loop l acc = match l with
	| [] -> List.rev acc
	| hd::tl -> loop tl ((List.length hd)::acc)
in loop lsts []

let lengths_fold lsts = List.fold_left (fun x l -> (List.length l)::x) [] lsts

(* Practice w/ map: ('a -> 'b) -> 'a list -> 'b list *)
(* In this case, 'a = 'a list *)
(* 'b = int *)
(* Our function must take 'a list -> int... List.length comes to mind *)
let lengths_lib lsts = List.map (fun ls -> List.length ls) lsts

(* Write a function find_first_value (lst : ('a * 'b) list) (x : 'a) : 'b option 
that eval- uates to Some z for the first pair (y,z) in the list such that x equals y. 
Return None if no such pair exists *)

let rec find_first_value_rec lst x = match lst with
| [] -> None
| (y,z)::tl -> if (x = y) then Some z else find_first_value_rec tl x 

let find_first_value_fold lst x = 
	let f opt (y,z) = match opt with 
	| None -> if x = y then Some z else None 
	| Some _ -> opt
in List.fold_left f None lst

(* Map: ('a -> 'b) -> 'a list -> 'b list *)
let find_first_value_lib lst x = 
	let optList = List.map (fun (y,z) -> if x = y then Some z else None) 
in List.find (fun a -> match a with None -> false | _ -> true) (optList lst)


(* Exercise 3: Write the functions using any of the three strategies *)
(* a. confirm_outputs (fs : ('a -> 'b) list) (i : 'a) (o : 'b): bool that evaluates to true if and only if each function in fs evaluates to o when applied to i *)
let rec confirm_outputs fs i o = match fs with
| [] -> true
| hd::tl -> if ((hd i) = o) then confirm_outputs tl i o else false

(* b. total_length (lsts : 'a list list) : int that evaluates to the total number of elements in all the lists in lsts *)
let total_length lsts = List.fold_left (fun acc elem -> acc + (List.length elem)) 0 lsts

(* c. find_last_value (lst : ('a * 'b) list) (x : 'a) : 'b option that evaluates to Some z for the last pair (y,z) in the list such that x equals y. 
Return None if x does not appear in any pair. *)
let find_last_value_lib lst x = 
	let optList = List.map (fun (y,z) -> if x = y then Some z else None) 
in List.find (fun a -> match a with None -> false | _ -> true) (List.rev (optList lst))

let find_last_value lst x = 
	let f opt (y,z) = match opt with 
	| None -> if x = y then Some z else None 
	| Some n -> if (x = y) then Some z else opt
in List.fold_left f None lst

(* d. median (lst : ’a list) : ’a option that evaluates to the median of the list. 
For even sized lists, choose the lesser of the two middle objects. For empty lists, return None *)

let median lst = let sorted = List.sort (fun x y -> if x = y then 0 else if x > y then 1 else -1) in
	let even i = if ((i mod 2) = 0) then true else false in
	let nth i = List.nth (sorted lst) (((List.length lst) / 2) + i) in
	let upper = nth 1 in
	let lower = nth 0 in
	let unevenMiddle = List.nth (sorted lst) ((List.length lst - 1) / 2)
in match lst with
| [] -> None
| _ -> if (even (List.length lst)) then (if upper > lower then Some upper else Some lower) else Some unevenMiddle



