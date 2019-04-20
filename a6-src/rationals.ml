open Matrix
type rational = 
  | Int of int
  | Frac of int * int  
  | Float of float
module RATIONAL : NUM with type t = rational = struct
  let tolerance = 10.0 ** 6.0
  (** A module for working with t rationalbers *)
  exception ArithmeticError
  (* AF: A value of type t is a Int of type int, a Frac of type int * int,
   * where the first number is the numerator, and the second number is the
   * denominator, or a Float of type float *)
  (* RI: The denominator of a Frac may not be zero *)
  type t = rational
  (** [zero] is [Int 0] *)
  let zero = Int 0
  (** [one] is [Int 1] *)
  let one = Int 1

  let make_Float = fun (f:float) -> Float f
  let make_Int = fun (i:int) -> Int i
  let make_Frac = fun (n:int) (d:int) -> Frac (n,d)

  (** [rep_ok r] is the rational number if the reprsentation meets the R.I. and
    * fails otherwise *)
  let rep_ok = function
    | Int i -> Int i
    | Float f -> Float f
    | Frac (n,d) -> begin
        if d = 0 then raise ArithmeticError
        else if d < 0 then Frac (-n, -d)
        else Frac (n,d)
      end
  (** [gcd n1 n2] is the largest integer that divides n1 and n2 with no remainder *)
  let rec gcd = fun (n1:int) (n2:int) ->
    if n2 = 0 then n1
    else gcd n2 (n1 mod n2) 
  (** [simplify (n,d)] is the simplified t rational using the greatest
    * common divisor of [n] and [d] *)
  let simplify = function
    | Int i -> Int i
    | Float f -> Float f
    | Frac (n,d) -> begin
        if d = 0 then raise ArithmeticError else
          let d' = gcd n d in
          let n,d = n/d',d/d' in
          if d = 1 then Int n 
          else if d = -1 then Int (-n)
          else if d < 0 then Frac (-n, -d)
          else Frac (n,d)
      end
  (** [to_float (n,d)] is the float representation of a rational number *)
  let to_float = function
    | Int i -> float_of_int i
    | Float f -> f
    | Frac (n,d) -> (float_of_int n) /. (float_of_int d)
  (** [to_exact f] is the exact value representation of a rational number *)
  let to_exact = function
    | Int i -> Int i
    | Frac (n,d) -> Frac (n,d)
    | Float f -> begin
        print_endline (string_of_float tolerance);
        simplify (Frac (int_of_float (f *. tolerance), int_of_float tolerance))
      end
  (** [add (n1,d1) (n2,d2)] is the sum of two rational numbers *)
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
    | Float f1, Float f2 -> Float (f1 +. f2)
    | Float f, b -> add (Float f) (Float (to_float b))
    | a, Float f -> add (Float (to_float a)) (Float f)

  (** [mul (n1,d1) (n2,d2)] is the product of two t rational numbers *)
  let rec mul = fun (r1:t) (r2:t) ->
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> Int (i1 * i2)
    | Int i, Frac (n,d) -> simplify (Frac ((i*n),d))
    | Frac (n,d), Int i -> simplify (Frac ((i*n),d))
    | Frac (n1,d1), Frac (n2,d2) -> simplify (Frac ((n1*n2),(d1*d2)))
    | Float f1, Float f2 -> Float (f1 *. f2)
    | Float f, b -> mul (Float f) (Float (to_float b))
    | a, Float f -> mul (Float (to_float a)) (Float f)

  (** [div (n1.d1) (n2.d2)] is the quotient of [r1] by [r2] *)
  let rec div = fun (r1:t) (r2:t) ->
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> mul r1 (Frac (1,i2))
    | Int i, Frac (n,d) -> mul r1 (Frac (d,n))
    | Frac (n,d), Int i -> mul (Frac (n,d)) (Frac (1,i))
    | Frac (n1,d1), Frac (n2,d2) -> mul (Frac (n1,d1)) (Frac (d2,n2))
    | Float f1, Float f2 -> Float (f1 /. f2)
    | Float f, b -> div (Float f) (Float (to_float b))
    | a, Float f -> div (Float (to_float a)) (Float f)

  (** [pow r1 r2] is r1 raised to the r2 power. 
      Require: 
      [r1] and [r2] are of type t (rationals)
  *)
  let pow = fun (base:t) (pow:t) -> 
    match pow with 
    | Frac (n,d) -> Float ((to_float base) ** (to_float pow))
    | Float f -> Float ((to_float base) ** f)
    | Int int_pow ->
      let rec helper = fun (pow:int) -> 
        if pow=0 then Int 1 else 
          mul base (helper (pow - 1))
      in if int_pow>=0 then helper int_pow else 
        div (Int 1) (helper (abs int_pow))

  (** [sub (n1,d1) (n2,d2)] is the difference of two rational numbers *)
  let rec sub = fun (r1:t) (r2:t) ->
    match rep_ok r1, rep_ok r2 with
    | Int i1, Int i2 -> Int (i1 - i2)
    | Int i, Frac (n,d) -> add r1 (Frac (-n,d))
    | Frac (n,d), Int i -> add r1 (Int (-i))
    | Frac (n1,d1), Frac (n2,d2) -> add r1 (Frac (-n2,d2))
    | Float f1, Float f2 -> Float (f1 -. f2)
    | Float f, b -> sub (Float f) (Float (to_float b))
    | a, Float f -> sub (Float (to_float a)) (Float f)
  (** [to_string (n,d)] is the string repsentation of a t rationalber *)
  let rec to_string = function
    | Int i -> string_of_int i
    | Frac (n,d) -> (string_of_int n)^"/"^(string_of_int d)
    | Float f -> to_string (to_exact (Float f))
  (** [compare r1 r2] is the order of the rational numbers [r1] and [r2]
    * If [r1] is less than [r2], is LT
    * If [r1] is greater than [r2], is GT
    * If [r1] is equal to [r2], is EQ *)
  let rec compare = fun (r1:t) (r2:t) -> 
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
        else if (n mod d) < 0 then GT
        else EQ
      end
    | Frac (n,d), Int i -> begin
        if (n/d) > i then GT
        else if (n/d) < i then LT
        else if (n mod d) > 0 then GT
        else if (n mod d) < 0 then LT
        else EQ
      end
    | Frac (n1,d1), Frac (n2,d2) -> begin
        if (n1/d1) < (n2/d2) then LT
        else if (n1/d1) > (n2/d2) then GT
        else if (n1 mod d1) < (n2 mod d2) then LT
        else if (n1 mod d1) > (n2 mod d2) then GT
        else EQ
      end
    | Float f1, Float f2 ->
      let acc = 1.0 /. tolerance in (* accurate to the millionth *)
      if f1 <= f2 +. acc && f1 >= f2 -. acc then EQ
      else if f1 < f2 then LT
      else if f1 > f2 then GT
      else EQ
    | Float f, b -> compare (Float f) (Float (to_float b))
    | a, Float f -> compare (Float (to_float a)) (Float f)
  (** [abs f] is the absolute value of the rational number [f] *)
  let abs = function
    | Int i -> Int (Pervasives.abs i)
    | Float f -> Float (Pervasives.abs_float f)
    | Frac (n,d) -> Frac (Pervasives.abs n, Pervasives.abs d)
  (** [neg f] is the negation of [f] *)
  let neg = function
    | Int i -> Int (-i)
    | Float f -> Float (-.f)
    | Frac (n,d) -> Frac (-n,d)
  (** [format fmt f] is the formatted version of [f] using [fmt] *)
  let format = fun (fmt:Format.formatter) (f:t) -> 
    Format.fprintf fmt "%s" (to_string f)
end