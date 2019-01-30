type value =
  | None
  | Int of int
  | Bool of bool

type env = (string * int) list

val lookup : env -> string -> value

type cont =
  (* arith_cont *)
  | Lookup of string
  | Plus_with of int
  | Times_with of int
  | Compile_then_plus of Ast.arith_exp
  | Compile_then_times of Ast.arith_exp
  (* returns to eval_bool *)
  | Bool_op_with of Ast.bool_op * int
  | Compile_then_bool_op of Ast.bool_op * Ast.arith_exp
  (* returns to eval_statement *)
  | Update_env_then of string

  (* bool_cont *)
  | Not
  | And_with of bool
  | Or_with of bool
  | Compile_then_and of Ast.bool_exp
  | Compile_then_or of Ast.bool_exp
  (* returns to eval_statement *)
  | Branch of Ast.statement * Ast.statement
  | Loop of Ast.statement * Ast.statement

  (* statement_cont *)
  | Compile_then of Ast.statement

val interp_arith : env * value -> cont list -> env * value
val compile_arith : env * Ast.arith_exp -> cont list -> env * value

val interp_bool : env * value -> cont list -> env * value
val compile_bool : env * Ast.bool_exp -> cont list -> env * value

val interp_statement: env -> cont list  -> env * value
val compile_statement : env * Ast.statement -> cont list -> env * value

val eval : Ast.statement -> env * value
