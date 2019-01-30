type value = int

type env = string -> int

val eval_arith : env -> Ast.arith_exp -> int

val eval_bool : env -> Ast.bool_exp -> bool

val eval_statement : Ast.statement -> env
