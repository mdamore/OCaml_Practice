(* In this exercise we will represent non-negative integers in binary using the following types: *)
type bit = Zero | One 
type bits = bit list

(* By convention, values of type bits should not have leading Zeros. For example, we represent 0 as the empty list [] and not [Zero] *)

(* bits_to_int : bits -> int *)
let bits_to_int (b : bits) =
	let rec loop l acc exp = match l with
	| [] -> int_of_float acc
	| hd::tl -> match hd with 
				| Zero -> loop tl acc (exp +. 1.0)
				| One -> loop tl (acc +. (2.0 ** exp)) (exp +. 1.0)
in loop b 0.0 0.0

(* int_to_bits : int -> bits *)
let int_to_bits number =
	let rec loop n bitAcc = if (n != 0) then 
								(if (n mod 2) = 0 then loop (n / 2) (Zero::bitAcc) 
									else loop ((n-1) / 2) (One::bitAcc)) 
							else bitAcc	
in loop number []

