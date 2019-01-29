(* 2: Defunctionalise *)
open Ast
;;

type value =
  | None
  | Int of int
  | Bool of bool
;;

let lookup env var =
  Int (List.Assoc.find_exn env ~equal:String.(=) var)
;;

type arith_cont =
  | Arith_Id
  | Lookup of string * arith_cont
  | Plus_with of int * arith_cont
  | Times_with of int * arith_cont
  | Compile_then_plus of arith_exp * arith_cont
  | Compile_then_times of arith_exp * arith_cont
  (* returns to eval_bool *)
  | Bool_op_with of bool_op * int * bool_cont
  | Compile_then_bool_op of bool_op * arith_exp * bool_cont
  (* returns to eval_statement *)
  | Compile_then_update_env of string * statement_cont

and bool_cont =
  | Bool_Id
  | Not of bool_cont
  | And_with of bool * bool_cont
  | Or_with of bool * bool_cont
  | Compile_then_and of bool_exp * bool_cont
  | Compile_then_or of bool_exp * bool_cont
  (* returns to eval_statement *)
  | Branch of statement * statement * statement_cont
  | Loop of statement * statement * statement_cont

and statement_cont =
  | Statement_Id
  | Compile_then of statement * statement_cont
;;

let rec interp_arith (env, int) =
  match int with None | Bool _ -> assert false | Int int ->
  function
  | Arith_Id -> (env, Int int)
  | Lookup (var, rest) -> interp_arith (env, lookup env var) rest
  | Plus_with (int2, rest) -> interp_arith (env, Int (int + int2)) rest
  | Times_with (int2, rest) -> interp_arith (env, Int (int * int2)) rest
  | Bool_op_with (op, int2, rest) ->
    let bool = match op with
      | Lt -> int < int2
      | Gt -> int > int2
      | Eq -> int = int2 in
    interp_bool (env, Bool bool) rest
  | Compile_then_plus (arith, rest) -> compile_arith (env, arith) (Plus_with (int, rest))
  | Compile_then_times (arith, rest) -> compile_arith (env, arith) (Times_with (int, rest))
  | Compile_then_bool_op (op, arith, rest) -> compile_arith (env, arith) (Bool_op_with (op, int, rest))
  | Compile_then_update_env (var, rest) -> interp_statement ((var, int) :: env) rest

and compile_arith (env, arith) rest =
  match arith with
  | Var var -> interp_arith (env, Int 0) (Lookup (var, rest))
  | Int int -> interp_arith (env, Int int) rest
  | Plus (a, b) -> compile_arith (env, a) (Compile_then_plus (b, rest))
  | Times (a, b) -> compile_arith (env, a) (Compile_then_times (b, rest))

and interp_bool (env, bool) =
  match bool with None | Int _ -> assert false | Bool bool ->
  function
  | Bool_Id -> (env, Bool bool)
  | Not rest -> interp_bool (env, Bool (not bool)) rest
  | And_with (bool2, rest) -> interp_bool (env, Bool (bool && bool2)) rest
  | Or_with (bool2, rest) -> interp_bool (env, Bool (bool || bool2)) rest
  | Compile_then_and (bool_exp, rest) -> compile_bool (env, bool_exp) (And_with (bool, rest))
  | Compile_then_or (bool_exp, rest) -> compile_bool (env, bool_exp) (Or_with (bool, rest))
  | Branch (true_, false_, rest) -> compile_statement (env, if bool then true_ else false_) rest
  | Loop (body, loop, rest) -> compile_statement (env, body) (Compile_then (loop, rest))

and compile_bool (env, bool_exp) rest =
  match bool_exp with
  | True -> interp_bool (env, Bool true) rest
  | False -> interp_bool (env, Bool false) rest
  | Not exp -> compile_bool (env, exp) (Not rest)
  | And (first, second) -> compile_bool (env, first) (Compile_then_and (second, rest))
  | Or (first, second) -> compile_bool (env, first) (Compile_then_or (second, rest))
  | Bool_op (first, op, second) ->
    compile_arith (env, first) (Compile_then_bool_op (op, second, rest))

and interp_statement env = function
  | Statement_Id -> (env, None)
  | Compile_then (second, rest) -> compile_statement (env, second) rest

and compile_statement (env, statement) rest =
  match statement with
  | Skip -> interp_statement env rest
  | Assign (var, arith_exp) -> compile_arith (env, arith_exp) (Compile_then_update_env (var, rest))
  | Seq (first, second) -> compile_statement (env, first) (Compile_then (second, rest))
  | If (cond, true_, false_) -> compile_bool (env, cond) (Branch (true_, false_, rest))
  | While (cond, body) as loop -> compile_bool (env, cond) (Loop (body, loop, rest))
;;

