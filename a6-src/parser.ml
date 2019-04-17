open Matrix
open ArrayMatrix
open Rationals

module RM = MAKE_MATRIX(RATIONAL)

open RM

(* #### STRING MATCH CASES #### *)
let strnum = "\([-]?[0-9]+\\?[0-9]*\)"
let strrow = "\(\[\([ ]+"^strnum^"[ ]+\)*\]\)"
let strmatrix = "\["^strrow^"*\]"
let stradd = "[ ]+add[ ]+"
let strsub = "[ ]+sub[ ]+" 
let strtranspose = "[ ]+transpose[ ]+"
let strinverse = "[ ]+inverse[ ]+"
let strdeterminant = "[ ]+determinant[ ]+"
let strrowspace = "[ ]+rowspace[ ]+"
let strnullspace = "[ ]+nullspace[ ]+"
let strcolspace = "[ ]+colspace[ ]+"
let strdot = "[ ]+dot[ ]+"
let strsolve = "[ ]+solve[ ]+"
let strlet = "[ ]+let[ ]+"
let strin = "[ ]+in[ ]+"

type typ =
  | TMatrix
  | TValue
  | TNone
  | TList of typ

type expr =
  | Matrix of matrix
  | Value of rational
  | Var of string
  | Let of (string * expr * expr)
  | Add of (expr * expr)
  | Sub of (expr * expr)
  | Dot of (expr * expr)
  | Mul of (expr * expr)
  | Div of (expr * expr)
  | Scale of (expr * expr)
  | Solve of (expr * expr)
  | Determinant of expr
  | Inverse of expr
  | Transpose of expr
  | Reduce of expr
  | Eigenvalues of expr
  | Eigenvectors of expr
  | NullSpace of expr
  | ColSpace of expr
  | List of expr list
  | Lookup of (expr * int)

let rec string_of_typ = fun (t:typ) ->
  match t with 
  | TMatrix -> "TMatrix"
  | TValue -> "TValue"
  | TNone -> "TNone"
  | TList t -> "TList of "^string_of_typ t

let rec typecheck = fun (e:expr) ->
  match e with
  | Matrix _ -> TMatrix
  | Value _ -> TValue
  | Var name -> failwith ("Unassigned variable: "^name)
  | Let (name,e1,e2) -> 
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> te2 then 
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else te1
  | Add (e1,e2) -> 
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> te2 then 
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else te1
  | Sub (e1,e2) -> 
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> te2 then 
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else te1
  | Mul (e1,e2) ->
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> te2 then 
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else te1
  | Dot (e1, e2) ->
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> te2 || te1 <> TMatrix then 
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else te1
  | Div (e1,e2) ->
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> te2 || te1 <> TValue then 
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else TValue
  | Scale (e1, e2) ->
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> TValue || te2 <> TMatrix then 
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else TMatrix
  | Solve (e1, e2) ->
    let te1, te2 = typecheck e1, typecheck e2 in
    if te1 <> TMatrix || te2 <> TMatrix then
      failwith ("The type of e1: "^(string_of_typ te1)^" does not match "^(string_of_typ te2))
    else TList TMatrix
  | Determinant e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TValue
  | Inverse e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TMatrix
  | Transpose e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TMatrix
  | Reduce e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TMatrix
  | Eigenvalues e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TList TValue
  | Eigenvectors e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TList TMatrix
  | NullSpace e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TList TMatrix
  | ColSpace e ->
    if typecheck e <> TMatrix then
      failwith ("The type of e is: "^(string_of_typ (typecheck e))^" which does is not TMatrix")
    else TList TMatrix
  | List l -> begin
    let t = ref None in
    List.iter (fun e -> 
      t := 
      match !t with 
      | None -> Some (typecheck e)
      | Some v -> 
        if v = typecheck e then !t 
        else failwith "List does not have coherent type") l;
    match !t with
    | None -> TList TNone
    | Some t -> TList t
    end 
  | Lookup (e, idx) -> begin
    match typecheck e with
    | TList t -> TList t
    | _ -> failwith "List does not have coherent type"
    end 

let rec subst = fun (context:string * expr) (e:expr) ->
  match e with
  | Matrix _ -> e
  | Value _ -> e
  | Var x ->
    if x = fst context then snd context else failwith (x^" is unbound")
  | Let (x,y,z) -> subst context (subst (x,y) z)
  | Add (e1, e2) -> Add ((subst context e1), (subst context e2))
  | Sub (e1, e2) -> Add ((subst context e1), (subst context e2))
  | Dot (e1, e2) -> Dot ((subst context e1), (subst context e2))
  | Mul (e1, e2) -> Mul ((subst context e1), (subst context e2))
  | Div (e1, e2) -> Div ((subst context e1), (subst context e2))
  | Scale (e1, e2) -> Scale ((subst context e1), (subst context e2))
  | Solve (e1, e2) -> Solve ((subst context e1), (subst context e2))
  | Determinant e -> subst context e
  | Inverse e -> subst context e
  | Transpose e -> subst context e
  | Reduce e -> subst context e
  | ColSpace e -> subst context e
  | NullSpace e -> subst context e
  | Eigenvalues e -> subst context e
  | Eigenvectors e -> subst context e
  | List l -> begin
    let memo = ref [] in
    List.iter (fun e -> 
      memo := (subst context e)::(!memo)) l;
    List (List.rev (!memo))
    end
  | Lookup (l,idx) -> Lookup (subst context l,idx)

let rec eval = fun (e:expr) -> 
  ignore (typecheck e);
  match e with
  | Matrix _ -> e
  | Value _ -> e
  | Var x -> failwith (x^ " is unbound")
  | Let (x,y,z) -> eval (subst (x,y) z)
  | Add (e1, e2) -> begin
    match eval e1, eval e2 with
    | Matrix m1, Matrix m2 -> Matrix (add m1 m2)
    | Value v1, Value v2 -> Value (RATIONAL.add v1 v2)
    | _, _ -> failwith "type mismatch"
    end
  | Sub (e1, e2) -> begin
    match eval e1, eval e2 with
    | Matrix m1, Matrix m2 -> Matrix (subtract m1 m2)
    | Value v1, Value v2 -> Value (RATIONAL.sub v1 v2)
    | _, _ -> failwith "type mismatch"
    end
  | Dot (e1, e2) -> begin
    match eval e1, eval e2 with
    | Matrix m1, Matrix m2 -> Value (dot m1 m2)
    | _, _ -> failwith "type mismatch"
    end
  | Mul (e1, e2) -> begin
    match eval e1, eval e2 with
    | Matrix m1, Matrix m2 -> Matrix (mul m1 m2)
    | Value v1, Value v2 -> Value (RATIONAL.mul v1 v2)
    | _, _ -> failwith "type mismatch"
    end
  | Div (e1, e2) -> begin
    match eval e1, eval e2 with
    | Value v1, Value v2 -> Value (RATIONAL.div v1 v2)
    | _, _ -> failwith "type mismatch"
    end
  | Scale (e1, e2) -> begin
    match eval e1, eval e2 with
    | Value v, Matrix m -> Matrix (scale v m)
    | _, _ -> failwith "type mismatch"
    end
  | Solve (e1, e2) -> begin
    match eval e1, eval e2 with
    | Matrix m1, Matrix m2 -> 
      let memo = ref [] in
      List.iter (fun m -> 
        memo := (Matrix m)::(!memo)
      ) (solve m1 m2); List !memo
    | _, _ -> failwith "type mismatch"
    end
  | List l -> begin
    let memo = ref [] in
    List.iter (fun e -> 
      memo := (eval e)::(!memo)) l;
    List (List.rev (!memo))
    end
  | Determinant e -> begin
    match eval e with
    | Matrix m -> Value (determinant m)
    | _ -> failwith "type mismatch"
    end
  | Inverse e -> begin
    match eval e with
    | Matrix m -> Matrix (inverse m)
    | _ -> failwith "type mismatch"
    end
  | Transpose e -> begin
    match eval e with
    | Matrix m -> Matrix (transpose m)
    | _ -> failwith "type mismatch"
    end
  | Reduce e -> begin
    match eval e with
    | Matrix m -> Matrix (reduce m)
    | _ -> failwith "type mismatch"
    end
  | ColSpace e -> begin
    match eval e with
    | Matrix m -> 
      let memo = ref [] in
      List.iter (fun m -> 
        memo := (Matrix m)::(!memo)
      ) (col_space m); List !memo
    | _ -> failwith "type mismatch"
    end
  | NullSpace e -> begin
    match eval e with
    | Matrix m -> 
      let memo = ref [] in
      List.iter (fun m -> 
        memo := (Matrix m)::(!memo)
      ) (null_space m); List !memo
    | _ -> failwith "type mismatch"
    end
  | Eigenvalues e -> begin
    match eval e with
    | Matrix m -> 
      let memo = ref [] in
      List.iter (fun v -> 
        memo := (Value v)::(!memo)
      ) (eigenvalues m); List !memo
    | _ -> failwith "type mismatch"
    end
  | Eigenvectors e -> begin
    match eval e with
    | Matrix m -> 
      let memo = ref [] in
      List.iter (fun m -> 
        memo := (Matrix m)::(!memo)
      ) (eigenvectors m); List !memo
    | _ -> failwith "type mismatch"
    end
  | Lookup (e, idx) -> begin
    let memo = ref None in 
    let e = match eval e with
    | List l -> 
      List.iteri (fun i e ->
      if i = idx then memo := Some e else ()
      ) l; !memo 
    | _ -> failwith "type mismatch" in
    match e with
    | None -> failwith "index out of bounds"
    | Some e -> eval e
    end

let rec expr_of_string = fun (s:string) -> 

  failwith "TODO"


