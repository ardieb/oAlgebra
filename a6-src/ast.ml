(* HEADER *)
open Matrix
open ArrayMatrix
open Rationals

module RM = MAKE_MATRIX(RATIONAL)
type num = RATIONAL.t
type matrix = RM.matrix
(* END HEADER *)

(* Functions for working with regex strings *)
(* matches an integer *)
let string_int = "[-]?[0-9]+"
(* matches a float *)
let string_float = Format.sprintf "%s\\.%s" string_int string_int
(* matches a fraction *)
let string_frac = Format.sprintf "%s/%s" string_int string_int

(** [frac_of s] is the fraction matched from string [s]. 
  * Fails if the string does not match a fraction.
  * Ex: frac_of "5/3" = Frac (5,3), frac_of "4" fails *)
let frac_of = fun (s:string) ->
  match Str.split (Str.regexp "/") s with
  | n::d::[] -> Frac (int_of_string n, int_of_string d)
  | _ -> failwith "Invalid fraction"

(** [float_of s] is the float matched from string [s].
  * Fails if the string does not match a float.
  * Ex: float_of "5.0" = Float 5.0, float_of "5" fails *)
let float_of = fun (s:string) -> 
  Float (float_of_string s)

(** [int_of s] is the integer matched from string [s].
  * Fails if the string does not match an integer.
  * Ex: int_of "5" = Int 5, int_of "5.0" fails *)
let int_of = fun (s:string) -> 
  Int (int_of_string s)

(** [num_of s] is the numeric value matched from string [s].
  * Fails if the string does not match a numeric value.
  * Ex: num_of "5.0" = Float 5.0 *)
let num_of = fun (s:string) ->
  if Str.string_match (Str.regexp string_frac) s 0 then
    frac_of (Str.matched_string s)
  else if Str.string_match (Str.regexp string_float) s 0 then
    float_of (Str.matched_string s)
  else if Str.string_match (Str.regexp string_int) s 0 then
    int_of (Str.matched_string s)
  else failwith "Nan"  

(** [row_of s] is the row (list) matched by string [s] 
  * Ex: row_of "[1 2 3]" = [Int 1; Int 2; Int 3] *)
let row_of = fun (s:string) ->
  Str.global_replace (Str.regexp "\\[\\|\\]") "" s |>
  Str.split (Str.regexp "[ ]+") |> 
  List.fold_left (fun acc e ->
    (num_of e)::acc
  ) [] |> List.rev

(** [matrix_of s] is the matrix matched by string [s].
  * Fails if the rows of the matrix have different lengths or if the 
  * type of elements are not numeric 
  * Ex: matrix_of "[1 2 3]; [1 2 3]" = 
    [[Int 1; Int 2; Int 3]; [Int 1; Int 2; Int 3]] *)
let matrix_of = fun (s:string) ->
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

(** [unaryop] is a type that classifies operations with one argument *)
type unaryop =
| Transpose
| Inverse
| Det
| Rowspace
| Nullspace
| Colspace
| Reduce
| QRFactor
| LuDecomp

(** [binaryop] is a type that classifies operations with two arguments*)
type binaryop = 
| Add
| Sub
| Div
| Mul
| Dot
| Solve
| Scale
| ChangeBasis
| OrthProject
| DistToBasis
| Decomp

(** [expr] is a type that classifies operations on matricies and numbers*)
type expr =
| Matrix of matrix
| Num of num
| Unary of (unaryop * expr)
| Binary of (expr * binaryop * expr)
| List of expr list

(** [typ] is a type that classfiies the type of an [expr] *)
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
    | QRFactor -> TList TMatrix
    | LuDecomp -> TMatrix
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
    | QRFactor -> let q,r = RM.qr_fact arg in
      List ((Matrix q)::(Matrix r)::[])
    | LuDecomp -> 
      let l,u = RM.lu_decomp arg in
      List ((Matrix l)::(Matrix u)::[])
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
    | Matrix oldbasis, ChangeBasis, Matrix newbasis -> 
      Matrix (RM.change_of_basis oldbasis newbasis)
    | Matrix vector, OrthProject, Matrix basis ->
      Matrix (RM.orth_proj basis vector)
    | Matrix vector, DistToBasis, Matrix basis ->
      Num (RM.distance basis vector)
    | Matrix vector, Decomp, Matrix basis ->
      let col_proj, orth_proj = RM.orth_decomp basis vector in
      List ((Matrix col_proj)::(Matrix orth_proj)::[])
    | _, _, _ -> failwith "Type mismatch"
    end in eval' e 

(** [format_expr e] is the printing function for the [e]xpr *)
let rec format_expr fmt = function
| Matrix m -> RM.format fmt m
| Num n -> RATIONAL.format fmt n
| Unary (op, e) -> begin
  match op with
  | Transpose -> Format.fprintf fmt "Transpose %a" format_expr e
  | Inverse -> Format.fprintf fmt "Inverse %a" format_expr e
  | Rowspace -> Format.fprintf fmt "Rowspace %a" format_expr e
  | Colspace -> Format.fprintf fmt "Colspace %a" format_expr e
  | Nullspace -> Format.fprintf fmt "Nullspace %a" format_expr e
  | Det -> Format.fprintf fmt "Determinant %a" format_expr e
  | Reduce -> Format.fprintf fmt "Reduce %a" format_expr e
  | QRFactor -> Format.fprintf fmt "QR Factor %a" format_expr e
  | LuDecomp -> Format.fprintf fmt "LU Decompose %a" format_expr e
  end
| Binary (e1, op, e2) -> begin
  match op with
  | Add -> Format.fprintf fmt "%a + %a" 
    format_expr e1 format_expr e2
  | Sub -> Format.fprintf fmt "%a - %a" 
    format_expr e1 format_expr e2
  | Div -> Format.fprintf fmt "%a / %a" 
    format_expr e1 format_expr e2
  | Mul -> Format.fprintf fmt "%a * %a" 
    format_expr e1 format_expr e2
  | Dot -> Format.fprintf fmt "%a Dot %a" 
    format_expr e1 format_expr e2
  | Solve -> Format.fprintf fmt "%a = %a" 
    format_expr e1 format_expr e2
  | Scale -> Format.fprintf fmt "%a %a" 
    format_expr e1 format_expr e2
  | ChangeBasis -> Format.fprintf fmt "%a changed to %a" 
    format_expr e1 format_expr e2
  | OrthProject -> Format.fprintf fmt "%a projected onto %a" 
    format_expr e1 format_expr e2
  | DistToBasis -> Format.fprintf fmt "%a distance to %a" 
    format_expr e1 format_expr e2
  | Decomp -> Format.fprintf fmt "%a decomposed on %a"
    format_expr e1 format_expr e2
  end
| List exprs -> 
  List.iter (fun elt ->
    Format.fprintf fmt "%a" format_expr elt
  ) exprs 
  




  
