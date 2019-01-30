type value = int

type env = string -> int

val eval_arith : env * Ast.arith_exp ->  (env * value -> 'a) -> 'a

val eval_bool : env * Ast.bool_exp -> (env * bool -> 'a) -> 'a

val eval_statement : env * Ast.statement -> (env -> 'a) -> 'a

val eval : Ast.statement -> env
