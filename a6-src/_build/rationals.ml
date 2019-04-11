open Matrix
module RATIONAL : (NUM with type t := int * int) = struct
  (** A module for working with t numbers *)
  exception ArithmeticError

  (* AF: a value of type [t] is an int * int pair where the first integer
   * represents the numerator and the second integer represents the denominator *)
  (* RI: the denominator of the t may not be equal to zero *)
  type t = int * int
  (** [zero] is the zero repsentation of a rational number *)
  let zero = (0, 1)
  (** [one] is the one representation of a rational number *)
  let one = (1, 1)
  (** [neg_one] is the negative one representation of a rational number *)
  let neg_one = ((-1), 1)
  (** [rep_ok r] fails if r is not a valid t number, does nothing otherwise *)
  let rep_ok = fun ((_,d):t) ->
    if d = 0 then raise ArithmeticError else ()
  (** [gcd n1 n2] is the largest number that divides n1 and n2 evenly *)
  let rec gcd = fun (n1:int) (n2:int) ->
    if n2 = 0 then n1
    else gcd n2 (n1 mod n2) 
  (** [simplify (n,d)] is the simplified t number using the greatest
    * common divisor of [n] and [d] *)
  let simplify = fun ((n,d):t)->
    rep_ok (n,d);
    let m = gcd n d in
    n / m, d / m
  (** [add (n1,d1) (n2,d2)] is the sum of two t numbers *)
  let add = fun ((n1,d1):t) ((n2,d2):t) ->
    rep_ok (n1,d1); rep_ok (n2,d2);
    simplify (n1*d2+n2*d1, d1*d2)
  (** [mul (n1,d1) (n2,d2)] is the product of two t numbers *)
  let mul = fun ((n1,d1):t) ((n2,d2):t) ->
    rep_ok (n1,d1); rep_ok (n2,d2);
    simplify (n1*n2, d1*d2)
  (** [div (n1.d1) (n2.d2)] is the quotient of two t numbers *)
  let div = fun ((n1,d1):t) ((n2,d2):t) ->
    rep_ok (n1,d1); rep_ok (n2,d2);
    mul (n1,d1) (d2,n2)
  (** [sub (n1,d1) (n2,d2)] is the difference of two t numbers *)
  let sub = fun ((n1,d1):t) ((n2,d2):t) ->
    rep_ok (n1,d1); rep_ok (n2,d2);
    add (n1,d1) (-n2,d2)
  (** [to_float (n,d)] is the float representation of a t number *)
  let to_float = fun ((n,d):t) ->
    (float_of_int n) /. (float_of_int d)
  (** [to_string (n,d)] is the string repsentation of a t number *)
  let to_string = fun ((n,d):t) ->
    string_of_int n ^ "/" ^ string_of_int d
  (** [compare f1 f2] is the order of the rational numbers f1 and f2.
    * If f1 is less than f2, is LT
    * If f1 is greater than f2, is GT
    * If f1 is equal to f2, is EQ *)
  let compare = fun ((n1,d1):t) ((n2,d2):t) -> 
    match Pervasives.compare (n1/d1) (n2/d2) with
    | n when n = 0 -> begin
        match Pervasives.compare (n1 mod d1) (n2 mod d2) with
        | n when n = 0 -> EQ
        | n when n < 0 -> LT
        | _ -> GT
      end
    | n when n < 0 -> LT
    | _ -> GT
  (** [abs f] is the absolute value of the rational number [f] *)
  let abs = fun ((n,d):t) ->
    Pervasives.abs (n), Pervasives.abs (d)
  (** [format fmt f] is the formatted version of [f] using [fmt] *)
  let format = fun (fmt:Format.formatter) (f:t) -> 
    Format.fprintf fmt "%s" (to_string f)
end