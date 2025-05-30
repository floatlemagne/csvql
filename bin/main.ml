open Lab13

let () =
  if Array.length Sys.argv = 2 then (
    let filename = Sys.argv.(1) in
    try
      let program_ast = Frontend.parse_file filename in
      Evaluator.eval_prog program_ast
    with
    | Evaluator.RuntimeError msg ->
        Printf.eprintf "Runtime Error: %s\n" msg;
        exit 1
    | Frontend.SyntaxError msg ->
        Printf.eprintf "Syntax Error: %s\n" msg;
        exit 1
    | Sys_error msg ->
        Printf.eprintf "System Error: %s\n" msg;
        exit 1
    | e ->
        Printf.eprintf "An unexpected error occurred: %s\n"
          (Printexc.to_string e);
        exit 1)
  else Printf.eprintf "Usage: %s <program_file.dbq>\n" Sys.argv.(0)
