open Matrix
type rational =
  | Int of int
  | Frac of int * int
  | Float of float
module RATIONAL : NUM with type t = rational