type arith_exp =
  | Var of string
  | Int of int
  | Plus of arith_exp * arith_exp
  | Times of arith_exp * arith_exp
;;

type bool_op =
  | Lt
  | Gt
  | Eq
;;

type bool_exp =
  | True
  | False
  | Not of bool_exp
  | And of bool_exp * bool_exp
  | Or of bool_exp * bool_exp
  | Bool_op of arith_exp * bool_op * arith_exp
;;

type closure = {
  arg : string;
  body : statement;
  return : arith_exp;
}
and statement =
  | Skip
  | Assign of string * ( string option * arith_exp, closure ) Either.t
  | Seq of statement * statement
  | If of bool_exp * statement * statement
  | While of bool_exp * statement
  | Call of string * arith_exp
;;
