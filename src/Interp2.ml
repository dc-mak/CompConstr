open Ast
;;

type instr =
  | Lookup of string
  | Push_int of int
  | Plus
  | Times
  | Push_bool of bool
  | Not
  | And
  | Or
  | Less_than
  | Greater_than
  | Equals
  | Assign of string
  | If of instr list * instr list
  | While of instr list * instr list
  | Pop_env
  | Pop_stack
  | Push_closure of compiled_closure
  | Call

and compiled_closure = {
  arg : string;
  body : instr list;
}
;;

let rec compile_arith = function
  | Var var -> [Lookup var]
  | Int int -> [Push_int int]
  | Plus (a, b) -> compile_arith a @ compile_arith b @ [Plus]
  | Times (a, b) -> compile_arith a @ compile_arith b @ [Times]
;;

let rec compile_bool = function
  | True -> [Push_bool true]
  | False -> [Push_bool false]
  | Not exp -> compile_bool exp @ [Not]
  | And (first, second) -> compile_bool first @ compile_bool second @ [And]
  | Or (first, second) -> compile_bool first @ compile_bool second @ [Or]
  | Bool_op (first, op, second) ->
    let op = match op with
      | Lt -> Less_than
      | Gt -> Greater_than
      | Eq -> Equals in
    compile_arith first @ compile_arith second @ [op]

let rec compile_statement = function
  | Skip ->  []
  | Assign (var, (First (None, exp))) ->
    compile_arith exp @ [Assign var]

  | Assign (var, (First (Some func, exp))) ->
    compile_arith exp @ [ Lookup func; Call; Pop_env; Assign var ]

  | Assign (var, Second { arg; body; return } ) ->
    [Push_closure { arg; body = compile_statement body @ compile_arith return }; Assign var ]

  | Call (func, exp) ->
    compile_arith exp @ [ Lookup func; Call; Pop_env; Pop_stack (* no assign *)]

  | Seq (first, second) ->
    compile_statement first @ compile_statement second

  | If (cond, true_, false_) ->
    compile_bool cond @ [If (compile_statement true_, compile_statement false_)]

  | While (cond, body) ->
    [While (compile_bool cond, compile_statement body)]
;;

type value =
  | Int of int
  | Bool of bool
  | Closure of compiled_closure
;;

type env =
  (string * (int, compiled_closure) Either.t) list
;;

let lookup env var =
  match List.Assoc.find_exn env ~equal:String.(=) var with
  | Either.First int -> Int int
  | Second closure -> Closure closure
;;

type stack =
  value list
;;

exception Malformed_stack
;;

let one_bool = function
  | Bool a :: stack -> (a, stack)
  | _ ->  raise Malformed_stack
;;

let two_bools = function
  | Bool a :: Bool b :: stack -> (a, b, stack)
  | _ ->  raise Malformed_stack
;;

let one_int = function
  | Int a :: stack -> (a, stack)
  | _ -> raise Malformed_stack
;;

let two_ints = function
  | Int a :: Int b :: stack -> (a, b, stack)
  | _ ->  raise Malformed_stack
;;

let closure_and_int = function
  | Closure closure  :: Int int :: stack -> (closure, int, stack)
  | _ -> raise Malformed_stack
;;

let rec interp (env, stack) = function
  | [] -> (env, stack)

  | Lookup var :: rest -> interp (env, lookup env var :: stack) rest

  | Push_int int :: rest -> interp (env, Int int :: stack) rest

  | Plus :: rest ->
    let (a, b, stack) = two_ints stack in
    interp (env, Int (a + b) :: stack) rest

  | Times :: rest ->
    let (a, b, stack) = two_ints stack in
    interp (env, Int (a * b) :: stack) rest

  | Push_bool bool :: rest -> interp (env, Bool bool :: stack) rest
  | Not :: rest ->
    let (bool, stack) = one_bool stack in
    interp (env, Bool (not bool) :: stack) rest

  | And :: rest ->
    let (a, b, stack) = two_bools stack in
    interp (env, Bool (a && b) :: stack) rest

  | Or :: rest ->
    let (a, b, stack) = two_bools stack in
    interp (env, Bool (a || b) :: stack) rest

  | Less_than :: rest ->
    let (a, b, stack) = two_ints stack in
    interp (env, Bool (a < b) :: stack) rest

  | Greater_than :: rest ->
    let (a, b, stack) = two_ints stack in
    interp (env, Bool (a > b) :: stack) rest

  | Equals :: rest ->
    let (a, b, stack) = two_ints stack in
    interp (env, Bool (a = b) :: stack) rest

  | Assign var :: rest ->
    let (a, stack) = one_int stack in
    interp ((var, First a) :: env, stack) rest

  | If (true_, false_) :: rest ->
    let (a, stack) = one_bool stack in
    interp (env, stack) ((if a then true_ else false_) @ rest)

  | While (cond, body) as loop :: rest ->
    interp (env, stack) (cond @ [If (body @ [loop], [])] @ rest)

  | Push_closure closure :: rest ->
    interp (env, (Closure closure) :: stack) rest

  | Call :: rest ->
    let ({arg; body}, int, stack) = closure_and_int stack in
    interp ((arg, First int) :: env, stack ) (body @ rest)

  | Pop_stack :: rest ->
    interp (env, List.tl_exn stack) rest

  | Pop_env :: rest ->
    interp (List.tl_exn env, stack) rest

;;

let eval statement =
  interp ([], []) (compile_statement statement)
;;
