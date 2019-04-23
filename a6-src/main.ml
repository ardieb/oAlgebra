open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec run () = 
  let ok = 0 in 
  ANSITerminal.(
    print_endline "Enter an expression > ";
    match read_line () with
    | exception End_of_file -> exit ok
    | s when s = "done" -> exit ok
    | s -> 
      print_endline (parse s |> eval |> Ast.string_of_expr);
      run ())

let _ = run ()