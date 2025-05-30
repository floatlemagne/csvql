{
open Parser (* Assuming 'parser.mly' generates 'parser.ml' *)
exception LexerError of string
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | [' ' '\t' '\n'] { read lexbuf } (* Skip whitespace *)
  | "project"      { PROJECT }
  | "from"         { FROM }
  | "join"         { JOIN }
  | "with"         { WITH }
  | "on"           { ON }
  | "rename"       { RENAME }
  | "to"           { TO }
  | "in"           { IN }
  | "load"         { LOAD }
  | "print"        { PRINT }
  | "save"         { SAVE }
  | ":="           { ASSIGN }
  | ";"            { SEMI }
  | ","            { COMMA }
  (* Add rules for Join, Rename, On, With later *)
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '"' ([^ '"' ]* as s) '"' { STRING s } (* Capture content of string literal *)
  | ident as s   { IDENT s }
  | eof            { EOF }
  | _ as c       { raise (LexerError (Printf.sprintf "Unexpected character: %c" c)) }

{
(* Footer - Can add helper functions here if needed *)
}