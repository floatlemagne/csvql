%{
open Ast (* Assuming your AST types are in ast.ml *)
%}

(* Declare tokens (add others as needed) *)
%token <string> IDENT
%token <string> STRING
%token ASSIGN SEMI EOF PRINT SAVE LOAD LPAREN RPAREN
%token PROJECT FROM COMMA
%token JOIN WITH ON
%token RENAME TO IN
(* Add tokens for Join, Rename, On, With later *)

%start program
%type <Ast.program> program
%type <Ast.table_expr> table_expr
%type <Ast.command> command
%type <Ast.names> names (* Type for the list of column names *)
%type <Ast.key> key (* Type for the join key column name *)
%type <(string * string) list> rename_list (* Type for list of (old, new) pairs *)
%type <(string * string)> rename_pair (* Type for a single (old, new) pair *)

%%

program:
  /* A program is a non-empty list of commands followed by EOF */
  | p=nonempty_list(command) EOF { p }
  ;

command:
  | x=IDENT ASSIGN t=table_expr SEMI { Assign(x, t) }
  | PRINT t=table_expr SEMI          { Print(t) }
  | SAVE t=table_expr s=STRING SEMI  { Save(t, s) }
  ;

table_expr:
  | x=IDENT                 { Var(x) }
  | LOAD s=STRING           { Load(s) }
  | PROJECT ns=names FROM t=table_expr { Project(ns, t) }
  | JOIN t1=table_expr WITH t2=table_expr ON k=key { Join(t1, t2, k) }
  | RENAME rl=rename_list IN t=table_expr { Rename(rl, t) }
  (* Add rules for Join, Rename later *)
  | LPAREN t=table_expr RPAREN { Parens(t) } /* Task 6 Parens already here */
  ;

names:
  /* Parses one or more STRINGs separated by COMMAs */
  | ns=separated_nonempty_list(COMMA, STRING) { ns }
  ;

key:
| k=STRING { k }
;

rename_list:
| rl=separated_nonempty_list(COMMA, rename_pair) { rl }
;

rename_pair:
| old_name=STRING TO new_name=STRING { (old_name, new_name) }
;

%%