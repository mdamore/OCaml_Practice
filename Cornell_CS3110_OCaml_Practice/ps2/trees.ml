type 'a bintree = Leaf | Node of 'a bintree * 'a * 'a bintree

let example_tree = Node (
					Node (
						Node (Leaf, 2, Leaf),
						4,
						Node (Leaf, 7, Node(Leaf, 8, Leaf))),
					3110,
					Node (Leaf,
						 6, 
						 Node (Node (Leaf, -3, Leaf), 14, Leaf)))

(* (a) Write a function tree_sum : int bintree -> int 
such that tree_sum t evaluates to the sum of the elements in t *)

let rec tree_sum t = match t with
|Leaf -> 0
|Node (l,v,r) -> v + (tree_sum l) + (tree_sum r) 

(* b. Write a function tree_mem : ’a -> ’a bintree -> bool 
that evalutates to true if the element exists in the tree and false otherwise *)
let rec tree_mem value tree = match tree with
|Leaf -> false
|Node (l,v,r) -> if (v = value) then true else (tree_mem value l || tree_mem value r)

(* (c) Write a function tree_preorder : ’a bintree -> ’a list 
such that tree_preorder t evaluates to a list containing the data in t ordered by preorder traversal *)

let tree_preorder tree = 
	let rec loop t acc = match t with
	|Leaf -> acc
	|Node (l,v,r) -> v::((loop l acc)@(loop r acc))
in loop tree []

(* (d) Write a function tree_inorder : ’a bintree -> ’a list 
such that tree_inorder t evaluates to a list containing the data in t ordered by inorder traversal. *)
let tree_inorder tree = 
	let rec loop t acc = match t with
	|Leaf -> acc
	|Node (l,v,r) -> (loop l acc)@[v]@(loop r acc)
in loop tree []


(* (e) Write a function tree_postorder : ’a bintree -> ’a list such that 
tree_postorder t eval- uates to a list containing the data in t ordered by postorder traversal. *)
let tree_postorder tree = 
	let rec loop t acc = match t with
	|Leaf -> acc
	|Node (l,v,r) -> (loop l acc)@(loop r acc)@[v]
in loop tree []


(* tree_fold : ’b -> (’a -> ’b -> ’b -> ’b) -> ’a bintree -> ’b *)

let rec tree_fold acc f tree = match tree with
|Leaf -> acc
|Node (l,v,r) -> f v (tree_fold acc f l) (tree_fold acc f r)

(* (b) Use tree_fold to reimplement the functions from the previous exercise in the file trees.ml. 
Name them tree_sum_fold, tree_mem_fold, tree_preorder_fold tree_inorder_fold,
and tree_postorder_fold. You may not use the rec keyword for these implementations *)

let tree_sum_fold = tree_fold 0 (fun v l r -> v + l + r)

let tree_mem_fold x = tree_fold false (fun v l r -> if (x = v) || l || r then true else false)

