(* Problem 1: SML Warm-UP *)

fun sumAll nil = 0 | sumAll (x::y) = x + sumAll y;

(* Problem 2: Interpreter in SML *)
datatype E = NUM of int | PLUS of E * E | TIMES of E * E
fun interp (NUM x) = x | interp (PLUS (e1,e2)) = (interp e1) + (interp e2)
	| interp (TIMES (e1, e2)) = (interp e1) * (interp e2)


(* Problem 3: Trees Revisited *)
datatype tree = NIL | CONS of (tree * tree) | LEAF of int;
fun treemap f NIL = NIL | treemap f (LEAF x) = (LEAF (f x))
	| treemap f (CONS(e1, e2)) = (CONS (treemap f e1, treemap f e2))



