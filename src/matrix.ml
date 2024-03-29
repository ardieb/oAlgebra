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
  val tolerance : float
  val make_Float : float -> t 
  val make_Int : int -> t 
  val make_Frac : int -> int -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val pow : t -> t -> t
  val sub : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val to_float : t -> float
  val to_string : t -> string
  val float_to_int : t -> t
end

module type MATRIX = sig
  module N : NUM
  type value = N.t
  exception MatrixError
  type matrix
  type solution = matrix * matrix list
  include Formattable with type t:= matrix
  val format_solution : Format.formatter -> solution -> unit
  val diagonal : int -> int -> matrix
  val make : int -> int -> value -> value list list -> matrix
  val dim : matrix -> int*int
  val transpose : matrix -> matrix
  val dot : matrix -> matrix -> value
  val mul : matrix -> matrix -> matrix
  val add : matrix -> matrix -> matrix
  val scale : value -> matrix -> matrix
  val inverse : matrix -> matrix
  val reduce : matrix -> matrix
  val augment : matrix -> matrix -> matrix
  val partition : int * int -> int * int -> matrix -> matrix 
  val solve : matrix -> matrix -> solution
  val determinant : matrix -> value
  val null_space : matrix -> matrix list
  val col_space : matrix -> matrix list
  val row_space : matrix -> matrix list
  val equals : matrix -> matrix -> bool
  val subtract : matrix -> matrix -> matrix
  val supp_matrix : matrix -> int -> int -> matrix
  val qr_fact : matrix -> matrix * matrix
  val magnitude : matrix -> value
  val normalize : matrix -> matrix
  val get : matrix -> int -> int -> value
  val change_of_basis : matrix -> matrix -> matrix
  val least_square : matrix -> matrix
  val orth_proj : matrix -> matrix -> matrix
  val distance : matrix -> matrix -> value
  val least_square : matrix -> matrix -> matrix
  val orth_decomp : matrix -> matrix -> matrix * matrix
  val lu_decomp : matrix -> matrix*matrix
end

module type MATRIX_MAKER = 
  functor (T:NUM) ->
    MATRIX with module N := T