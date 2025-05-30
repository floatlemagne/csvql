type names = string list
type key = string

type table_expr =
  | Var of string
  | Load of string
  | Project of names * table_expr
  | Join of table_expr * table_expr * key
  | Rename of (string * string) list * table_expr
  | Parens of table_expr

type command =
  | Assign of string * table_expr
  | Print of table_expr
  | Save of table_expr * string

type program = command list
