type value =
  | None
  | Int of int
  | Bool of bool

type env = (string * int) list

val lookup : env -> string -> value

type arith_cont =
  | Arith_Id
  | Lookup of string * arith_cont
  | Plus_with of int * arith_cont
  | Times_with of int * arith_cont
  | Compile_then_plus of Ast.arith_exp * arith_cont
  | Compile_then_times of Ast.arith_exp * arith_cont
  (* returns to eval_bool *)
  | Bool_op_with of Ast.bool_op * int * bool_cont
  | Compile_then_bool_op of Ast.bool_op * Ast.arith_exp * bool_cont
  (* returns to eval_statement *)
  | Update_env_then of string * statement_cont

and bool_cont =
  | Bool_Id
  | Not of bool_cont
  | And_with of bool * bool_cont
  | Or_with of bool * bool_cont
  | Compile_then_and of Ast.bool_exp * bool_cont
  | Compile_then_or of Ast.bool_exp * bool_cont
  (* returns to eval_statement *)
  | Branch of Ast.statement * Ast.statement * statement_cont
  | Loop of Ast.statement * Ast.statement * statement_cont

and statement_cont =
  | Statement_Id
  | Compile_then of Ast.statement * statement_cont

val interp_arith : env * value -> arith_cont -> env * value
val compile_arith : env * Ast.arith_exp -> arith_cont -> env * value

val interp_bool : env * value -> bool_cont -> env * value
val compile_bool : env * Ast.bool_exp -> bool_cont -> env * value

val interp_statement : env -> statement_cont -> env * value
val compile_statement : env * Ast.statement -> statement_cont -> env * value

val eval : Ast.statement -> env * value
