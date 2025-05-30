exception RuntimeError of string

module StringTable = Hashtbl.Make (String)

let default_size = 16

type table = string list list

let has_unique_elements (lst : string list) : bool =
  let sorted = List.sort String.compare lst in
  let rec check = function
    | [] | [ _ ] -> true
    | x :: y :: rest -> if x = y then false else check (y :: rest)
  in
  check sorted

let find_index (elem : 'a) (lst : 'a list) : int option =
  let rec find idx = function
    | [] -> None
    | hd :: tl -> if hd = elem then Some idx else find (idx + 1) tl
  in
  find 0 lst

let rec remove_at n = function
  | [] -> []
  | hd :: tl -> if n = 0 then tl else hd :: remove_at (n - 1) tl

let has_duplicate_old_names (rename_list : (string * string) list) : bool =
  let old_names = List.map fst rename_list in
  not (has_unique_elements old_names)

module ProgramEvaluator () : sig
  val eval_prog : Ast.program -> unit
  val eval_command : Ast.command -> unit
  val eval_texpr : Ast.table_expr -> table
end = struct
  let state : table StringTable.t = StringTable.create default_size

  open Ast

  let rec eval_prog = function
    | cmd :: cmds ->
        eval_command cmd;
        eval_prog cmds
    | [] -> ()

  and eval_command = function
    | Print t -> Csv.print_readable (eval_texpr t)
    | Assign (x, t) ->
        let table_value = eval_texpr t in
        StringTable.replace state x table_value
    | Save (t, f) -> (
        try
          let table_data = eval_texpr t in
          Csv.save f table_data
        with Sys_error msg ->
          raise
            (RuntimeError ("File system error saving to '" ^ f ^ "': " ^ msg)))

  and eval_texpr = function
    | Var x -> begin
        match StringTable.find_opt state x with
        | None -> raise (RuntimeError ("unbound variable: " ^ x))
        | Some t -> t
      end
    | Load f -> begin
        try
          let raw_table = Csv.load f in

          if raw_table = [] then
            raise
              (RuntimeError
                 ("Cannot load table: file '" ^ f
                ^ "' is empty or contains no header row."));

          if not (Csv.is_square raw_table) then
            raise
              (RuntimeError
                 ("Cannot load table: file '" ^ f ^ "' is not rectangular."));

          let header = List.hd raw_table in
          if not (has_unique_elements header) then
            raise
              (RuntimeError
                 ("Cannot load table: file '" ^ f
                ^ "' has duplicate column names in the header."));

          raw_table
        with Sys_error msg ->
          raise (RuntimeError ("File system error loading '" ^ f ^ "': " ^ msg))
      end
    | Project (selected_names, source_texpr) ->
        let source_table = eval_texpr source_texpr in
        if source_table = [] then
          raise
            (RuntimeError
               "Project: cannot project from an empty table (no header).");
        let source_header = List.hd source_table in
        let source_data = List.tl source_table in
        if selected_names = [] then
          raise (RuntimeError "Project: must select at least one column.");
        if not (has_unique_elements selected_names) then
          raise
            (RuntimeError
               "Project: selected column list contains duplicate names.");
        let indices =
          try
            List.map
              (fun name ->
                match find_index name source_header with
                | None ->
                    raise
                      (RuntimeError
                         ("Project: column name '" ^ name
                        ^ "' not found in source table."))
                | Some idx -> idx)
              selected_names
          with RuntimeError msg -> raise (RuntimeError msg)
        in
        let new_header = selected_names in
        let select_cols_from_row row =
          try List.map (fun index -> List.nth row index) indices
          with Failure _ | Invalid_argument _ ->
            raise
              (RuntimeError
                 "Internal error during projection: row structure inconsistent.")
        in
        let new_data = List.map select_cols_from_row source_data in
        new_header :: new_data
    | Join (texpr1, texpr2, key_col_name) ->
        let table1 = eval_texpr texpr1 in
        let table2 = eval_texpr texpr2 in
        if table1 = [] || table2 = [] then
          raise
            (RuntimeError "Join: cannot join empty tables (missing header).");
        let header1 = List.hd table1 in
        let data1 = List.tl table1 in
        let header2 = List.hd table2 in
        let data2 = List.tl table2 in
        let key_idx1 =
          match find_index key_col_name header1 with
          | None ->
              raise
                (RuntimeError
                   ("Join: key column '" ^ key_col_name
                  ^ "' not found in first table's header."))
          | Some idx -> idx
        in
        let key_idx2 =
          match find_index key_col_name header2 with
          | None ->
              raise
                (RuntimeError
                   ("Join: key column '" ^ key_col_name
                  ^ "' not found in second table's header."))
          | Some idx -> idx
        in
        let header2_non_key = remove_at key_idx2 header2 in
        let final_header = header1 @ header2_non_key in
        if not (has_unique_elements final_header) then
          raise
            (RuntimeError
               "Join: resulting table would have duplicate column names \
                (non-key columns conflict).");
        let result_data =
          List.fold_left
            (fun acc_rows row1 ->
              try
                let key_val1 = List.nth row1 key_idx1 in
                if key_val1 = "" then acc_rows
                else
                  let matching_rows_from_t2 =
                    List.fold_left
                      (fun acc_matches row2 ->
                        try
                          let key_val2 = List.nth row2 key_idx2 in
                          if key_val1 = key_val2 then
                            let row2_non_key_data = remove_at key_idx2 row2 in
                            let combined_row = row1 @ row2_non_key_data in
                            combined_row :: acc_matches
                          else acc_matches
                        with Failure _ | Invalid_argument _ -> acc_matches)
                      [] data2
                  in
                  acc_rows @ List.rev matching_rows_from_t2
              with Failure _ | Invalid_argument _ -> acc_rows)
            [] data1
        in
        final_header :: result_data
    | Rename (rename_list, texpr) ->
        let source_table = eval_texpr texpr in
        if source_table = [] then
          raise
            (RuntimeError "Rename: cannot rename columns in an empty table.");
        let source_header = List.hd source_table in
        let data = List.tl source_table in
        if has_duplicate_old_names rename_list then
          raise
            (RuntimeError
               "Rename: duplicate column specified in 'old name' list.");
        let applied_old_names = ref [] in
        let new_header =
          List.map
            (fun current_col_name ->
              match
                List.find_opt
                  (fun (old_name, _) -> old_name = current_col_name)
                  rename_list
              with
              | None -> current_col_name
              | Some (old_name, new_name) ->
                  applied_old_names := old_name :: !applied_old_names;
                  new_name)
            source_header
        in
        List.iter
          (fun (old_name, _) ->
            if not (List.mem old_name !applied_old_names) then
              raise
                (RuntimeError
                   ("Rename: column '" ^ old_name
                  ^ "' not found in the table header.")))
          rename_list;
        if not (has_unique_elements new_header) then
          raise
            (RuntimeError
               "Rename: operation results in duplicate column names in the \
                header.");
        new_header :: data
    | Parens t -> eval_texpr t
end

let eval_prog prog =
  let module PE = ProgramEvaluator () in
  PE.eval_prog prog
