open Matrix
open ArrayMatrix
open Rationals

module RM = MAKE_MATRIX(RATIONAL)

open RM

type token =
  | Add of (rational * rational)
  | Mul of (rational * rational)
  | Div of (rational * rational)
  | Sub of (rational * rational)
  | Abs of (rational)
  | Neg of (rational)
  | Transpose of (matrix)
  | Scale of (rational * matrix)
  | AddM of (matrix * matrix)
  | MulM of (matrix * matrix)
  | NullSpace of (matrix)
  | ColSpace of (matrix)
  | RowSpace of (matrix)
  | Solve of (matrix * matrix)
  | Inverse of (matrix * matrix)

let token_of_string = fun (s:string) ->
  let num = "\([-]?[0-9]*\\?[0-9]*\)" in 
  let row = "\(\[[ ]+" ^ num ^ "[ ]+\]\)" in
  let mat = "\[" ^ row ^ "*\]" in 
  let match_num = Str.regexp num in
  let match_matrix = Str.regexp mat in
  let match_addm = Str.regexp 
  ("[ ]+add[ ]+" ^ mat ^ " " ^ mat) in
  let match_subm = Str.regexp 
  ("[ ]+sub[ ]+" ^ mat ^ " " ^ mat) in
  let match_add = Str.regexp 
  ("[ ]+add[ ]+" ^ num ^ " " ^ num) in
  let match_sub = Str.regexp 
  ("[ ]+sub[ ]+" ^ num ^ " " ^ num) in
  let match_div = Str.regexp 
  ("[ ]+div[ ]+" ^ num ^ " " ^ num) in
  let match_abs = Str.regexp 
  ("[ ]+abs[ ]+" ^ num ^ " " ^ num) in
  let match_transpose = Str.regexp 
  ("[ ]+transpose[ ]+" ^ mat) in
  let match_scale = Str.regexp 
  ("[ ]+scale[ ]+" ^ num ^ " " ^ mat) in
  let match_nullspace = Str.regexp 
  ("[ ]+nullspace[ ]+" ^ mat) in
  let match_rowspace = Str.regexp 
  ("[ ]+rowspace[ ]+" ^ mat) in 
  let match_colspace = Str.regexp 
  ("[ ]+colspace[ ]+" ^ mat) in
  let match_solve = Str.regexp 
  ("[ ]*solve[ ]*" ^ mat ^ " " ^ mat) in
  let match_inverse = Str.regexp 
  ("[ ]+inverse[ ]+" ^ mat) in
  let match_determinant = Str.regexp 
  ("[ ]+determinant[ ]+" ^ mat) in
  failwith "TODO"

let rec parse = fun (tokens:token list) -> failwith "TODO"

