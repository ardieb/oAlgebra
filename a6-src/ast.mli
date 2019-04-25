open Matrix
open Rationals
module MAT : MATRIX with module N := RATIONAL 

type value = MAT.value
type matrix = MAT.matrix

val num_of : string -> value
val matrix_of : string -> matrix

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

type expr =
| Matrix of matrix
| Num of value
| Unary of (unaryop * expr)
| Binary of (expr * binaryop * expr)
| List of expr list

type typ =
| TMatrix
| TNum
| TList of typ

val typecheck : expr -> typ
val eval : expr -> expr
val format_expr : Format.formatter -> expr -> unit