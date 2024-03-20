type math_op =
  | Add
  | Subtract
  | Multiply
  | Divide

type compare_op =
  | Equal
  | LessThan

type logic_op =
  | And
  | Or

type ident = string
type type_ = Type.t

type expr =
  | Number of int
  | Boolean of bool
  | Ident of ident
  | BinaryMath of expr * math_op * expr
  | BinaryCompare of expr * compare_op * expr
  | BinaryLogic of expr * logic_op * expr
  | Not of expr
  | If of expr * expr * expr
  | Let of (ident * expr) list * expr
  | FnApp of expr * expr
  | FnExpr of ident * type_ * expr
  | Sequence of expr * expr
  | Mutate of ident * expr
  | ObjectExpr of (ident * expr) list
  | FieldAccess of expr * ident