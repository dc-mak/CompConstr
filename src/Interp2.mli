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

val compile_arith : Ast.arith_exp -> instr list
val compile_bool : Ast.bool_exp -> instr list
val compile_statement : Ast.statement -> instr list

type value =
  | Int of int
  | Bool of bool

type env = (string * int) list

val lookup : env -> string -> value

exception Malformed_stack

type stack = value list

val one_bool : stack -> bool * stack
val two_bools : stack -> bool * bool * stack

val one_int : stack -> int * stack
val two_ints : stack -> int * int * stack

val interp : env * stack -> instr list -> env * stack

val eval : Ast.statement -> env * stack
