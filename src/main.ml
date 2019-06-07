open Ast

(** [parse s] is the [expr] matched by string [s]. The rules that
  * define expressions are found in lexer.mll *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [run ()] is the main entry point for running the interpreter.
  * It loops until the user enters the line "done". If the user
  * enters an invalid expression, they are informed of the error
  * and prompted for a new input *)
let rec run () = 
  let ok = 0 in 
  ANSITerminal.(
    print_endline "Enter an expression >> ";
    match read_line () with
    | exception End_of_file -> exit ok
    | s when s = "done" -> exit ok
    | s -> 
      try parse s |> eval |> Ast.format_expr Format.str_formatter |> 
        Format.flush_str_formatter |> print_endline; run ()
      with _ -> print_endline "The entered expression was invalid"; run ())

let _ = run ()