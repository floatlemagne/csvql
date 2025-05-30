exception SyntaxError of string
(** Raised to indicate a syntax error detected during parsing. The string
    contains a message that attempts to describe where and what the error is. *)

val lex : string -> Parser.token list
(** [lex str] uses the lexer defined in "lexer.mll" to transform [str] into a
    token list. *)

val lex_file : string -> Parser.token list
(** [lex_file] is like [lex], except that the program is read from the file
    named [f]. *)

val parse : string -> Ast.program
(** [parse str] uses the parser and lexer defined in "parser.mly" and
    "lexer.mll" to transform [str] into an AST. Raises: [SyntaxError] if the
    program is not grammatically well-formed. *)

val parse_file : string -> Ast.program
(** [parse_file f] is like [parse], except that the program is read from the
    file named [f]. *)

val parse_buffer : Lexing.lexbuf -> Ast.program
(** [parse_buffer lexbuf] parses a CsvQL program from a standard OCaml lexer
    buffer [lexbuf]. *)
