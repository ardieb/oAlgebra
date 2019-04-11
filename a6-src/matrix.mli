type order = LT | GT | EQ

module type Comparable = sig
  type t
  val compare : t -> t -> order
end

module type Formattable = sig 
  type t
  val format : Format.formatter -> t -> unit 
end

module type NUM = sig
  type t  
  include Comparable with type t := t
  include Formattable with type t := t
  exception ArithmeticError
  val zero : t
  val one : t
  val neg_one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val sub : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val to_float : t -> float
  val to_string : t -> string
end

module type MATRIX = sig
  module N : NUM
  exception MatrixError
  type v = N.t
  type matrix
  val diagonal : int -> int -> matrix
  val make : int -> int -> v -> v list list -> matrix
  val dim : matrix -> int*int
  val transpose : matrix -> matrix
  val dot : matrix -> matrix -> v
  val mul : matrix -> matrix -> matrix
  val add : matrix -> matrix -> matrix
  val scale : v -> matrix -> matrix
  val inverse : matrix -> matrix
  val eigenvalues : matrix -> v list
  val eigenvectors : matrix -> matrix list
  val reduce : matrix -> matrix
  val augment : matrix -> matrix -> matrix
  val partition : int * int -> int * int -> matrix -> matrix 
  val solve : matrix -> matrix -> matrix
  val determinant : matrix -> v
end

module type MATRIX_MAKER = 
  functor (T:NUM) ->
    MATRIX with module N := T