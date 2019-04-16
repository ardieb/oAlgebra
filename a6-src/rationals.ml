open Matrix
type rational = 
  | Int of int
  | Frac of int * int 
module RATIONAL : NUM with type t = rational = struct
  (** A module for working with t numbers *)
  exception ArithmeticError

  (* AF: a value of type [t] is an int * int pair where the first integer
   * represents the numerator and the second integer represents the denominator *)
  (* RI: the denominator of the t may not be equal to zero and the denominator may not be negative *)
  type t = rational
  (** [zero] is the zero repsentation of a rational number *)
  let zero = Int 0
  (** [one] is the one representation of a rational number *)
  let one = Int 1
  (** [rep_ok r] fails if r is not a valid t number, does nothing otherwise *)
  let rep_ok = function
    | Int i -> Int i
    | Frac (n,d) -> begin
        if d = 0 then raise ArithmeticError
        else if d < 0 then Frac (-n, -d)
        else Frac (n,d)
      end
  (** [gcd n1 n2] is the largest number that divides n1 and n2 evenly *)
  let rec gcd = fun (n1:int) (n2:int) ->
    if n2 = 0 then n1
    else gcd n2 (n1 mod n2) 
  (** [simplify (n,d)] is the simplified t number using the greatest
    * common divisor of [n] and [d] *)
  let simplify = function
    | Int i -> Int i
    | Frac (n,d) -> begin
        if d = 0 then raise ArithmeticError else
          let d' = gcd n d in
          let n,d = n/d',d/d' in
          if d = 1 then Int n 
          else if d = -1 then Int (-n)
          else if d < 0 then Frac (-n, -d)
          else Frac (n,d)
      end
  (** [add (n1,d1) (n2,d2)] is the sum of two t numbers *)
  let rec add = fun (r1:t) (r2:t) ->
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> Int (i1 + i2)
    | Int i, Frac (n,d) -> begin
        let i1 = i * d in
        simplify (Frac ((n + i1),d))
      end
    | Frac (n,d), Int i -> begin
        let i1 = i * d in
        simplify (Frac ((n + i1),d))
      end
    | Frac (n1,d1), Frac (n2,d2) -> simplify (Frac (((n1*d2) + (n2*d1)),(d1*d2)))
  (** [mul (n1,d1) (n2,d2)] is the product of two t numbers *)
  let mul = fun (r1:t) (r2:t) ->
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> Int (i1 * i2)
    | Int i, Frac (n,d) -> simplify (Frac ((i*n),d))
    | Frac (n,d), Int i -> simplify (Frac ((i*n),d))
    | Frac (n1,d1), Frac (n2,d2) -> simplify (Frac ((n1*n2),(d1*d2)))
  (** [div (n1.d1) (n2.d2)] is the quotient of r1 by r2 *)
  let div = fun (r1:t) (r2:t) ->
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> mul r1 (Frac (1,i2))
    | Int i, Frac (n,d) -> mul r1 (Frac (d,n))
    | Frac (n,d), Int i -> mul (Frac (n,d)) (Frac (1,i))
    | Frac (n1,d1), Frac (n2,d2) -> mul (Frac (n1,d1)) (Frac (d2,n2))
  (** [sub (n1,d1) (n2,d2)] is the difference of two t numbers *)
  let sub = fun (r1:t) (r2:t) ->
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> Int (i1 - i2)
    | Int i, Frac (n,d) -> add r1 (Frac (-n,d))
    | Frac (n,d), Int i -> add r1 (Int (-i))
    | Frac (n1,d1), Frac (n2,d2) -> add r1 (Frac (-n2,d2))
  (** [to_float (n,d)] is the float representation of a t number *)
  let to_float = function
    | Int i -> float_of_int i
    | Frac (n,d) -> (float_of_int n) /. (float_of_int d)
  (** [to_string (n,d)] is the string repsentation of a t number *)
  let to_string = function
    | Int i -> string_of_int i
    | Frac (n,d) -> (string_of_int n)^"/"^(string_of_int d)
  (** [compare f1 f2] is the order of the rational numbers f1 and f2.
    * If f1 is less than f2, is LT
    * If f1 is greater than f2, is GT
    * If f1 is equal to f2, is EQ *)
  let compare = fun (r1:t) (r2:t) -> 
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> begin
        if i1 > i2 then GT
        else if i1 < i2 then LT
        else EQ
      end
    | Int i, Frac (n,d) -> begin
        if i > (n/d) then GT
        else if i < (n/d) then LT
        else if (n mod d) > 0 then LT
        else EQ
      end
    | Frac (n,d), Int i -> begin
        if (n/d) > i then GT
        else if (n/d) < i then LT
        else if (n mod d) > 0 then GT
        else EQ
      end
    | Frac (n1,d1), Frac (n2,d2) -> begin
        if (n1/d1) < (n2/d2) then LT
        else if (n1/d1) > (n2/d2) then GT
        else if (n1 mod d1) < (n2 mod d2) then LT
        else if (n1 mod d1) > (n2 mod d2) then GT
        else EQ
      end
  (** [abs f] is the absolute value of the rational number [f] *)
  let abs = function
    | Int i -> Int (Pervasives.abs i)
    | Frac (n,d) -> Frac (Pervasives.abs n, Pervasives.abs d)
  (** [neg f] is the negation of [f] *)
  let neg = function
    | Int i -> Int (-i)
    | Frac (n,d) -> Frac (-n,d)
  (** [format fmt f] is the formatted version of [f] using [fmt] *)
  let format = fun (fmt:Format.formatter) (f:t) -> 
    Format.fprintf fmt "%s" (to_string f)
end
