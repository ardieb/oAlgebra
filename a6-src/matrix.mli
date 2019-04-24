(** [order] is the order comparison returned by [Comparable.compare x y] *)
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
  (** [ArithmeticError] is raised when division by zero occurs or sqrt -1 *)
  exception ArithmeticError
  (** [zero] is the number zero *)
  val zero : t
  (** [one] is the number one *)
  val one : t
  val tolerance : float
  (** [make_X arg ...] makes a rational, either a Float or Int or Frac from 
    * OCaml number types like int and float *)
  val make_Float : float -> t 
  val make_Int : int -> t 
  val make_Frac : int -> int -> t
  (** [add x y] is the sum of [x] and [y] *)
  val add : t -> t -> t
  (** [mul x y] is the product of [x] and [y] *)
  val mul : t -> t -> t
  (** [div x y] is the quotient of [x] and [y] *)
  val div : t -> t -> t
  (** [pow x y] is x raised to the yth power *)
  val pow : t -> t -> t
  (** [sub x y] is the difference of [x] and [y] *)
  val sub : t -> t -> t
  (** [abs x] is the absolute value of [x] s*)
  val abs : t -> t
  (** [neg x] is -[x] *)
  val neg : t -> t
  (** [to_float x] is [x] as a float *)
  val to_float : t -> float
  (** [to_string x] is [x] as a string *)
  val to_string : t -> string
  (** turns Float into Int *)
  val float_to_int : t -> t
end

module type MATRIX = sig
  module N : NUM
  (** [MatrixError] is a general error raised during matrix computations *)
  exception MatrixError
  (** [value] is the type of value in the matrix *)
  type value = N.t
  (** [matrix] is the abstract type of a matrix *)
  type matrix
  (** [solution] is a particular vector solution paired with a list of 
    * linearly dependent solutions (i.e. the nullsace) *)
  type solution = matrix * matrix list
  include Formattable with type t := matrix
  (** [format_solution fmt sol] formats the solution for printing *)
  val format_solution : Format.formatter -> solution -> unit
  (** [diagonal rows cols] is the rows x cols matrix with 1's on the diagonal *)
  val diagonal : int -> int -> matrix
  (** [make rows cols init init_list] is the rows x cols matrix initialized with
    * value init and filled with values from [init_list] in order of position *)
  val make : int -> int -> value -> value list list -> matrix
  (** [dim m] is rows x cols dimension of the [m]atrix *)
  val dim : matrix -> int*int
  (** [transpose m] is the transepose of [m] *)
  val transpose : matrix -> matrix
  (** [dot u v] is the dot product of vectors [u] and [v] which are n x 1
    * matricies *)
  val dot : matrix -> matrix -> value
  (** [mul m1 m2] is the matrix product [m1] x [m2] *)
  val mul : matrix -> matrix -> matrix
  (** [add m1 m2] is the matrix sum [m1] + [m2] *)
  val add : matrix -> matrix -> matrix
  (** [scale c m] is c x [m]atrix *)
  val scale : value -> matrix -> matrix
  (** [inverse m] is the inverse of [m]. Raises matrix error if there is no
    * inverse *)
  val inverse : matrix -> matrix
  (** [reduce m] is the reduced echelon form of [m] *)
  val reduce : matrix -> matrix
  (** [augment m1 m2] is the matrix [m1][m2] where the columns of [m2] are 
    * appended to the columns of [m1] *)
  val augment : matrix -> matrix -> matrix
  (** [partition (x1,y1) (x2,y2) m] is the sub matrix of [m] from x1 -> x2 and
    * y1 -> y2 *)
  val partition : int * int -> int * int -> matrix -> matrix 
  (** [solve m1 v] is the solution to the linear equation [m]x = [v] 
    * This is a pair of a particular solution and the null space solution *)
  val solve : matrix -> matrix -> solution
  (** [determinant m] is the determinant of matrix [m] *)
  val determinant : matrix -> value
  (** [null_space m] is the null space of [m] *)
  val null_space : matrix -> matrix list
  (** [col_space m] is the column space of [m] *)
  val col_space : matrix -> matrix list
  (** [row_space m] is a list of vectors that form the row space of the [m]atrix
      which is the basis for the span of the rows of [m] *)
  val row_space : matrix -> matrix list  
  (** [equals m1 m2] is true if [m1] is structurally equal to [m2] and false
    * otherwise *)
  val equals : matrix -> matrix -> bool
  (** [subtract m1 m2] is the difference [m1] - [m2] *)
  val subtract : matrix -> matrix -> matrix
  (** [supp_matrix m row col] is the matrix [m] without values from [row] or 
    * from [col] *)
  val supp_matrix : matrix -> int -> int -> matrix
  (** [qr_fact m] is the QR factorization of m given as a pair of matrices*)
  val qr_fact : matrix -> matrix * matrix
  (** [magnitude vector] is the magnitude of [vector]*)
  val magnitude : matrix -> value
  (** [normalize v] is normalized vector [v] *)
  val normalize : matrix -> matrix
  (** [m r c] gets you the value in row [r] column [c] in matrix [m]*)
  val get : matrix -> int -> int -> value
  (** [change_of_bases b1 b2] is the standard matrix of the linear 
      transformation from basis [b1] to basis [b2] *)
  val change_of_basis : matrix -> matrix -> matrix
  (** [orth_proj basis vector] is the orthogonal projection of [vector] onto 
      the subspace spanned by the columns of [basis]*)
  val orth_proj : matrix -> matrix -> matrix
  (** [distance basis vector] is the distance from [vector] to the subspace 
      spanned by the columns of [basis] *)
  val distance : matrix -> matrix -> value

  val least_square : matrix -> matrix -> matrix

  (** [orth_decomp basis vector] is a tuple containing the orthogonal projection
      from [vector] to the columns spanned by [basis] and the projection of [vector]
      onto the orthogonal subspace of [basis] *)
  val orth_decomp : matrix -> matrix -> matrix * matrix
  (** [lu_decomp m] is a tuple containing matrices L and U, L being a lower
      triangular square matrix with 1's on the diagonal, U being the echelon form of 
      matrix [m], and the product of L and U being [m] *)
  val lu_decomp : matrix -> matrix*matrix
end

(** [MATRIX_MAKER] is the type of a functor for making a matrix *)
module type MATRIX_MAKER = 
  functor (T:NUM) ->
    MATRIX with module N := T