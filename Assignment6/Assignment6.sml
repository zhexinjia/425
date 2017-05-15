(* 1. Mitchell, Exercise 5.5 *)
datatype 'a tree = 
	LEAF of 'a 
	| NODE of 'a tree * 'a tree;

fun reduce func (LEAF x) = x
	|reduce func (NODE (e1, e2)) = func (reduce func (e1), reduce func (e2));

(* 2.(a) Mitchell, Exercise 5.6 *)
fun Curry f = fn x => fn y => f (x, y);	
fun UnCurry g = fn (x, y) => g x y;

(* 4. Mitchell, Exercise 5.8, part(a) and (b) *)
datatype 'a Seq = 
	Nil
	| Cons of 'a * (unit-> 'a Seq);
fun head (Cons (x, _)) = x;
fun tail (Cons (_, xs)) = xs();
fun BadCons (x, xs) = Cons (x, fn()=>xs);

(* 4 part (a) *)
fun merge(Nil, x) = x
	| merge(y, Nil) = y
	(*| merge(e1, e2) = if (head(e1)<head(e2)) then Cons(head(e1), fn()=>merge(tail(e1), e2)) else Cons(head(e2), fn()=>merge(tail(e2), e1));*)
	| merge(e1, e2) = Cons(head(e2), fn()=>merge(tail(e2), e1));

(* Used for testing
fun make_ints(f)=
	let
		fun make_pos(n)=Cons((n, f(n)), fn()=>make_pos(n+1))
		fun make_neg(n)=Cons((n, f(n)), fn()=>make_neg(n-1))
	in
		merge(make_pos(0), make_neg(~1))
	end;
val add1 = make_ints(fn(x)=>x+1);
val double = make_ints(fn(x)=>2*x);
fun apply (Cons ((x1, fx1), xs), x2) = 
	if (x1=x2) then fx1 else apply(xs(), x2);
apply(double, 7);
*)

(* 4 part (b) *)
fun compose(f,g) = (fn h => g(f h));
