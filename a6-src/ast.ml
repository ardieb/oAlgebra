open Matrix
open ArrayMatrix
open Rationals

module RM = MAKE_MATRIX(RATIONAL)

type num = RATIONAL.t
type matrix = RM.matrix

(* Functions for working with regex strings *)
(* matches an integer *)
let string_int = "[-]?[0-9]+"
(* matches a float *)
let string_float = Format.sprintf "%s.%s" string_int string_int
(* matches a fraction *)
let string_frac = Format.sprintf "%s/%s" string_int string_int

(** [frac_of s] is the fraction matched from string [s]. 
  * Fails if the string does not match a fraction *)
let frac_of = fun (s:string) ->
  match Str.split (Str.regexp "/") s with
  | n::d::[] -> Frac (int_of_string n, int_of_string d)
  | _ -> failwith "Invalid fraction"

(** [float_of s] is the float matched from string [s].
  * Fails if the string does not match a float *)
let float_of = fun (s:string) -> 
  Float (float_of_string s)

(** [int_of s] is the integer matched from string [s].
  * Fails if the string does not match an integer *)
let int_of = fun (s:string) -> 
  Int (int_of_string s)

(** [num_of s] is the numeric value matched from string [s].
  * Fails if the string does not match a numeric value *)
let num_of = fun (s:string) ->
  if Str.string_match (Str.regexp string_int) s 0 then
    int_of s
  else if Str.string_match (Str.regexp string_float) s 0 then
    float_of s
  else if Str.string_match (Str.regexp string_frac) s 0 then
    frac_of s
  else failwith "Nan"  

(** [row_of s] is the row (list) matched by string [s] *)
let row_of = fun (s:string) ->
  Str.global_replace (Str.regexp "\\[\\|\\]") "" s |>
  Str.split (Str.regexp "[ ]+") |> 
  List.fold_left (fun acc e ->
    (num_of e)::acc
  ) [] |> List.rev

(** [matrix_of s] is the matrix matched by string [s].
  * Fails if the rows of the matrix have different lengths or if the 
  * type of elements are not numeric *)
let matrix_of = fun (s:string) ->
  let s = String.sub s 1 (String.length s - 2) in
  let rows = Str.split (Str.regexp ";[ ]+\\|\\t") s in 
  let mat = List.fold_left (fun acc row -> 
    (row_of row)::acc
  ) [] rows in 
  let len = List.hd mat |> List.length in
  List.iter (fun row ->
    if List.length row != len then 
      failwith "Invalid matrix: row lengths differ" 
    else ()) mat;
  RM.make (List.length mat) len (Int 0) mat

(** [unaryop] is a variant type that classifies operations with one argument*)
type unaryop =
| Transpose
| Inverse
| Det
| Rowspace
| Nullspace
| Colspace
| Reduce
(** [binaryop] is a variant type that classifies operations with two arguments*)
type binaryop = 
| Add
| Sub
| Div
| Mul
| Dot
| Solve
| Scale
(** [expr] is a variant type that classifies operations on matricies and numbers*)
type expr =
| Matrix of matrix
| Num of num
| Unary of (unaryop * expr)
| Binary of (expr * binaryop * expr)
| List of expr list
(** [typ] is a variant type that classfiies the type of an [expr] *)
type typ =
| TMatrix
| TNum
| TList of typ
(** [string_of_type t] is the string form of [t]yp *)
let rec string_of_type = function
| TMatrix -> "TMatrix"
| TNum -> "TNum"
| TList t -> Format.sprintf "TList %s" (string_of_type t)
(** [typecheck e] is the type of the [e]xpr. Fails if the type is incoherent *)
let rec typecheck = function
| Matrix _ -> TMatrix
| Num _ -> TNum
| Unary (op,e) -> begin
  if typecheck e <> TMatrix then
    failwith "The argument provided is not type TMatrix" 
  else
    match op with
    | Transpose -> TMatrix
    | Inverse -> TMatrix
    | Det -> TNum
    | Rowspace -> TList TMatrix
    | Nullspace -> TList TMatrix
    | Colspace -> TList TMatrix
    | Reduce -> TMatrix
  end
| Binary (e1,Scale,e2) ->
  if (typecheck e1 <> TNum || typecheck e2 <> TMatrix) then
    failwith (
      Format.sprintf "Type %s does not match type %s" 
      (typecheck e1 |> string_of_type) (typecheck e2 |> string_of_type)
    )
  else TMatrix
| Binary (e1,_,e2) ->
  if typecheck e1 <> typecheck e2 then
    failwith (
      Format.sprintf "Type %s does not match type %s" 
      (typecheck e1 |> string_of_type) (typecheck e2 |> string_of_type)
    ) else typecheck e1
| List l ->
  let t' = typecheck (List.hd l) in
  List.iter (fun e -> 
    if 
      typecheck e <> t' 
    then 
      failwith (
        Format.sprintf "Type %s does not match type %s" 
        (t' |> string_of_type) (typecheck e |> string_of_type)
      ) else ()) l; t'
(** [eval e] is the evaluated expression (value) from [e].
  * All expressions step to either Matrix m or Num n eventually.
  * Will not during steps since typecheck is applied at the beginning. *)
let eval = fun (e:expr) -> 
  ignore (typecheck e);
  let rec eval' = function
  | Matrix m -> Matrix m
  | Num n -> Num n
  | List l -> List l
  | Unary (op, arg) -> begin
    let arg =
    match eval' arg with
    | Matrix m -> m
    | _ -> failwith "Type mismatch" in 
    match op with
    | Reduce -> Matrix (RM.reduce arg)
    | Inverse -> Matrix (RM.inverse arg)
    | Transpose -> Matrix (RM.transpose arg)
    | Det -> Num (RM.determinant arg)
    | Nullspace -> List (List.fold_right (fun e init ->
      (Matrix e)::init) (RM.null_space arg) [])
    | Colspace -> List (List.fold_right (fun e init -> 
      (Matrix e)::init) (RM.col_space arg) [])
    | Rowspace -> List (List.fold_right (fun e init ->
      (Matrix e)::init) (RM.row_space arg) [])
  end
  | Binary (arg1, Scale, arg2) ->
    let arg1, arg2 =
    match eval' arg1, eval' arg2 with
    | Num n, Matrix m -> n, m
    | _, _ -> failwith "Type mismatch" in
    Matrix (RM.scale arg1 arg2)
  | Binary (arg1, op, arg2) -> begin
    match eval' arg1, op, eval' arg2 with
    | Matrix m, Add, Matrix n -> Matrix (RM.add m n)
    | Num m, Add, Num n -> Num (RATIONAL.add m n)
    | Matrix m, Mul, Matrix n -> Matrix (RM.mul m n)
    | Num m, Mul, Num n -> Num (RATIONAL.mul m n)
    | Matrix m, Sub, Matrix n -> Matrix (RM.subtract m n)
    | Num m, Sub, Num n -> Num (RATIONAL.sub m n)
    | Num m, Div, Num n -> Num (RATIONAL.div m n)
    | Matrix m, Dot, Matrix n -> Num (RM.dot m n)
    | Matrix m, Solve, Matrix n -> 
      let sol = RM.solve m n in
      List ((Matrix (fst sol))::
      (List.fold_right (fun e init ->
      (Matrix e)::init) (RM.solve m n |> snd) []))
    | _, _, _ -> failwith "Type mismatch"
    end in eval' e 
(** [string_of_expr e] is the string form of the [e]xpr *)
let rec string_of_expr = function
| Matrix m -> RM.to_string m
| Num n -> RATIONAL.to_string n
| Unary (op, e) -> begin
  match op with
  | Transpose -> Format.sprintf "Transpose %s" (string_of_expr e)
  | Inverse -> Format.sprintf "Inverse %s" (string_of_expr e)
  | Rowspace -> Format.sprintf "Rowspace %s" (string_of_expr e)
  | Colspace -> Format.sprintf "Colspace %s" (string_of_expr e)
  | Nullspace -> Format.sprintf "Nullspace %s" (string_of_expr e)
  | Det -> Format.sprintf "Determinant %s" (string_of_expr e)
  | Reduce -> Format.sprintf "Reduce %s" (string_of_expr e)
  end
| Binary (e1, op, e2) -> begin
  match op with
  | Add -> Format.sprintf "%s + %s" (string_of_expr e1) (string_of_expr e2)
  | Sub -> Format.sprintf "%s - %s" (string_of_expr e1) (string_of_expr e2)
  | Div -> Format.sprintf "%s / %s" (string_of_expr e1) (string_of_expr e2)
  | Mul -> Format.sprintf "%s * %s" (string_of_expr e1) (string_of_expr e2)
  | Dot -> Format.sprintf "%s Dot %s" (string_of_expr e1) (string_of_expr e2)
  | Solve -> Format.sprintf "%s = %s" (string_of_expr e1) (string_of_expr e2)
  | Scale -> Format.sprintf "%s %s" (string_of_expr e1) (string_of_expr e2) 
  end
| List l -> 
  List.fold_left (fun acc elt ->
    acc^(string_of_expr elt)^" "
  ) "" l
  




  
