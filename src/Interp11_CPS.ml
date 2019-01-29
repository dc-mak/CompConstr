(* 1: CPS *)
open Ast
;;

type value =
  int
;;

type env =
  string -> int
;;

let rec eval_arith (env, arith) return =
  match arith with
  | Var var -> return (env, env var)
  | Int int -> return (env, int)
  | Plus (a, b) ->
    eval_arith (env, a) (fun (a_env, a_int) ->
        eval_arith (a_env, b) (fun (b_env, b_int) ->
            return (b_env, a_int + b_int)))
  | Times (a, b) ->
    eval_arith (env, a) (fun (a_env, a_int) ->
        eval_arith (a_env, b) (fun (b_env, b_int) ->
            return (b_env, a_int * b_int)))
;;

let rec eval_bool (env, bool) return =
  match bool with
  | True -> return (env, true)
  | False -> return (env, false)
  | Not exp ->
    eval_bool (env, exp) (fun (exp_env, bool) ->
      return (exp_env, not bool))
  | And (first, second) ->
    eval_bool (env, first) (fun (first_env, first_bool) ->
        eval_bool (first_env, second) (fun (second_env, second_bool) ->
            return (second_env, first_bool && second_bool)))
  | Or (first, second) ->
    eval_bool (env, first) (fun (first_env, first_bool) ->
        eval_bool (first_env, second) (fun (second_env, second_bool) ->
            return (second_env, first_bool || second_bool)))
  | Bool_op (first, op, second) ->
    let op = match op with
      | Lt -> (<)
      | Gt -> (>)
      | Eq -> (=) in
    eval_arith (env, first) (fun (first_env, first_arith) ->
        eval_arith (first_env, second) (fun (second_env, second_arith) ->
            return (second_env, op first_arith second_arith)))
;;

let rec eval_statement (env, statement) return =
  match statement with
  | Skip ->  return env
  | Assign (var, exp) ->
    eval_arith (env, exp) (fun (exp_env, exp_int) ->
        return (fun var' -> if String.(var = var') then exp_int else exp_env var'))
  | Seq (first, second) ->
    eval_statement (env, first) (fun first_env ->
        eval_statement (first_env, second) return)
  | If (cond, true_, false_) ->
    eval_bool (env, cond) (fun (cond_env, cond_bool) ->
        eval_statement (cond_env, if cond_bool then true_ else false_) return)
  | While (cond, body) as loop ->
    eval_bool (env, cond) (fun (cond_env, cond_bool) ->
      eval_statement (cond_env, if cond_bool then Seq (body, loop) else Skip) return)
;;

let eval_statement statement =
  eval_statement statement (fun x -> x)
;;
