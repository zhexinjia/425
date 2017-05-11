(* 1. Mitchell, Exercise 5.5 *)
datatype 'a tree = 
	LEAF of 'a 
	| NODE of 'a tree * 'a tree;

fun reduce func (LEAF x) = x
	|reduce func (NODE (e1, e2)) = func (reduce func (e1), reduce func (e2));
(*FIXME: Explain reduce function*)

(*
fun f (x : int, y : int) = x + y;
reduce f (NODE(NODE(LEAF 1, LEAF 2), LEAF 3));
*)

(* 2. Mitchell, Exercise 5.6 *)



(* 3. Mitchell, Exercise 5.7 *)
(* 4. Mitchell, Exercise 5.8, part(a) and (b) *)
(* 5. Mitchell, Exercise 6.2 *)
(* 6. Mitchell, Exercise 6.5 *)
(* 7. Mitchell, Exercise 6.6 *)
(* 8. Mitchell, Exercise 6.7 *)
