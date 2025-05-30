exception SyntaxError of string

let location_message lexbuf =
  let open Lexing in
  let start = lexeme_start_p lexbuf in
  let finish = lexeme_end_p lexbuf in
  Printf.sprintf "line %d, characters %d-%d" start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (finish.pos_cnum - finish.pos_bol)

let syntax_error_message lexbuf =
  Printf.sprintf "Syntax error, %s: unexpected token '%s'"
    (location_message lexbuf)
    (Lexing.lexeme lexbuf)

let parse_buffer lexbuf =
  try
    let ast = Parser.program Lexer.read lexbuf in
    ast
  with
  | Parser.Error -> raise (SyntaxError (syntax_error_message lexbuf))

let parse s =
  let lexbuf = Lexing.from_string s in
  parse_buffer lexbuf

let parse_file f =
  try
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f };
    let ast = parse_buffer lexbuf in
    close_in ic;
    ast
  with Sys_error msg ->
    raise (SyntaxError ("Cannot open file '" ^ f ^ "': " ^ msg))

let lex_buffer lexbuf =
  let[@tail_mod_cons] rec loop acc =
    match Lexer.read lexbuf with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | token -> loop (token :: acc)
  in
  loop []

let lex s =
  let lexbuf = Lexing.from_string s in
  lex_buffer lexbuf

let lex_file f =
   try
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f };
    let tokens = lex_buffer lexbuf in
    close_in ic;
    tokens
  with Sys_error msg ->
    Printf.printf "Error opening file '%s': %s\n" f msg;
    []
