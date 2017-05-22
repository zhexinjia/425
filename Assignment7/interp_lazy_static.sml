(*  Here's a skeleton file to help you get started on Interpreter 1.
 * Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/
 *)

use "type.sml";

(* Here is a result datatype *)
datatype result =
    RES_ERROR of string
  | RES_NUM   of int
  | RES_BOOL  of bool
  | RES_SUCC
  | RES_PRED
  | RES_ISZERO
  | RES_FUN   of (string * term)
  | RES_CLOSURE of (string * term * env) and env = Env of (string * (term * env)) list;

(* Here is a basic environment implementation *)
exception not_found;
(*datatype env = Env of (string * result) list*)

fun new_env() = Env(nil);
fun extend_env (Env(oldenv), id, value) = Env( (id, value):: oldenv);
fun lookup_env (Env(nil), id) = (print("Free Var!! "^id); raise not_found)
   |lookup_env (Env((id1,value1)::b), id) =  
        if (id1 = id) 
        then value1
	    else lookup_env(Env(b), id) ;

(*  Here's a partial skeleton of interp : (term * env) -> result.
    I've done the first case for you
*)
fun interp (exp, env) = 

  case exp of
    AST_NUM  x                    => RES_NUM x
  | AST_BOOL b                    => RES_BOOL b
  | AST_SUCC                      => RES_SUCC
  | AST_PRED                      => RES_PRED
  | AST_ISZERO                    => RES_ISZERO
  | AST_IF (exp1, exp2, exp3)     => (case interp(exp1, env) of
                                        RES_BOOL (true) => interp(exp2, env)
                                        |RES_BOOL (false) => interp(exp3, env)
                                        | _ => RES_ERROR "ERROR: exp1 not boolean.")

  | AST_APP (exp1, exp2)          => (case (interp(exp1, env)) of
    RES_CLOSURE (var, exp, static_env) => 
    let
      val new_env = extend_env(static_env, var, (exp2, static_env))
    in
      interp(exp, new_env)
    end
    | RES_SUCC => (case (interp(exp2, env)) of
      RES_NUM x => RES_NUM (x+1)
      | _ => RES_ERROR "Cannot increment non-int")
    | RES_PRED => (case (interp(exp2, env)) of
      RES_NUM 0 => RES_NUM 0
      | RES_NUM x => RES_NUM (x-1)
      | _ => RES_ERROR "Cannot decrement non-int")
    | RES_ISZERO => (case (interp(exp2, env)) of
      RES_NUM 0 => RES_BOOL true
      | RES_NUM x => RES_BOOL false
      | _ => RES_ERROR "Cannot apply to non-int")
    | _ => RES_ERROR "exp1 none function")
  | AST_ID name                   => let
    val (exp, lazy_env) = lookup_env(env, name)
  in
    interp(exp, lazy_env)
  end
  | AST_FUN (var, ty, exp)        => RES_CLOSURE(var, exp, env)

  | AST_LET (var, ty, exp1, exp2) => 
  let
    val new_env = extend_env(env, var, (exp1, env))
  in
    interp(exp2, new_env)
  end
  
  | AST_REC (var, ty, exp)        => (* You don't need to implement this *)
                                     RES_ERROR "Not yet implemented"