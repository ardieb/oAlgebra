open Matrix
type rational =
  | Int of int
  | Frac of int * int
module RATIONAL : NUM with type t = rational
