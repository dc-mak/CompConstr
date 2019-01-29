open Ast
;;

type value =
  int
;;

type env =
  string -> int
;;

let rec eval_arith env = function
  | Var var -> env var
  | Int int -> int
  | Plus (a, b) -> eval_arith env a + eval_arith env b
  | Times (a, b) -> eval_arith env a * eval_arith env b
;;

let rec eval_bool env = function
  | True -> true
  | False -> false
  | Not exp -> not (eval_bool env exp)
  | And (first, second) -> eval_bool env first && eval_bool env second
  | Or (first, second) -> eval_bool env first || eval_bool env second
  | Bool_op (first, op, second) ->
    let op = match op with
      | Lt -> (<)
      | Gt -> (>)
      | Eq -> (=) in
    op (eval_arith env first) (eval_arith env second)
;;

let rec eval_statement env = function
  | Skip ->  env
  | Assign (var, exp) ->
    let result = eval_arith env exp in
    fun var' -> if String.(var = var') then result else env var'
  | Seq (first, second) ->
    eval_statement (eval_statement env first) second
  | If (cond, true_, false_) ->
    eval_statement env (if eval_bool env cond then true_ else false_)
  | While (cond, body) as loop ->
    if eval_bool env cond then
      eval_statement (eval_statement env body) loop
    else
      env
;;

let eval_statement =
  eval_statement (fun x -> failwith @@ "Not found: " ^ x)
;;
