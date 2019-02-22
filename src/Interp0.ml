open Ast
;;

type value =
  int
;;

type env =
  string -> ((int, closure) Either.t) option
;;

exception Int_expected of string
;;

let get_int env var =
  match env var with
  | Some (Either.First int) -> int
  | None | Some (Second _) -> raise (Int_expected var)
;;

exception Function_expected of string
;;

let get_func env var =
  match env var with
  | None | Some (Either.First _) -> raise (Function_expected var)
  | Some Second func -> func
;;

let rec eval_arith (env : env) = function
  | Var var ->  get_int env var
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

  | Assign (var, First (None, exp)) ->
    let result = eval_arith env exp in
    fun find_me -> if String.(var = find_me) then Some (First result) else env find_me

  | Assign (var, Second closure ) ->
    fun find_me -> if String.(var = find_me) then Some (Second closure) else env find_me

  | Assign (var, First (Some func, exp)) ->

    (* Lookup function *)
    let { arg; body; return } = get_func env func in
    (* Eval argument *)
    let result = eval_arith env exp  in
    (* Save old value of arg lookup *)
    let old_val = env arg in
    (* Construct new env for eval'ing the function body *)
    let env var = if String.(arg = var) then Some (Either.First result) else env var in
    (* Eval body *)
    let env = eval_statement env body in
    (* Compute result *)
    let result = eval_arith env return in
    (* New env: var |-> result, arg |-> old_val *)
    fun find_me ->
      if String.(var = find_me) then Some (Either.First result) else
      if String.(arg = find_me) then old_val else env find_me

  | Seq (first, second) ->
    eval_statement (eval_statement env first) second
  | If (cond, true_, false_) ->
    eval_statement env (if eval_bool env cond then true_ else false_)
  | While (cond, body) as loop ->
    if eval_bool env cond then
      eval_statement (eval_statement env body) loop
    else
      env

  | Call (func, exp) ->

    (* Lookup function *)
    let { arg; body; return=_ } = get_func env func in
    (* Eval argument *)
    let result = eval_arith env exp  in
    (* Save old value of arg lookup *)
    let old_val = env arg in
    (* Construct new env for eval'ing the function body *)
    let env var = if String.(arg = var) then Some (Either.First result) else env var in
    (* Eval body, ignore result since called as statement *)
    let env = eval_statement env body in
    (* Restore environment with arg |-> old_val *)
    fun find_me -> if String.(arg = find_me) then old_val else env find_me

;;

exception Not_found of string
;;

let eval_statement =
  eval_statement (fun x -> raise (Not_found x))
;;
