(* THIS FILE IS MEANT TO BE A DEMONSTRATION OF THE USAGE OF THE INTERPRETER
 * ALONGSIDE OCAML CODE *)
open Ast
open Rationals

(** [parse s] is the [expr] matched by string [s]. The rules that
  * define expressions are found in lexer.mll *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* evaluates to [-3 -3]; [-3 -3] *)
let m = "((det [1 2]; [3 5]) scale [1 1]; [1 1]) - [2 2]; [2 2]"

(* This short example shows how you can integrate our string language with
 * OCaml code. Executed with make example from the command line *)
let _ =
  print_endline (
  match parse m |> eval with
  (* Determinant of a [-3 -3] [-3 -3] is 0 *)
  | Matrix m -> MAT.determinant m |> RATIONAL.to_string 
  | _ -> failwith "Not a matrix")