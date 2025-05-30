type table = string list list
(** The type [table] representing a table as processed by the evaluator. It is a
    list of rows where each row is a list of strings. The first row is
    conventionally assumed to be the header. *)

exception RuntimeError of string
(** [RuntimeError] is raised to indicate an error during the execution of a
    program. *)

val eval_prog : Ast.program -> unit
(** [eval_prog prog] evaluates [prog], causing its side effects to occur.
    Raises: [RuntimeError] if an error occurs. *)
