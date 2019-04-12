open OUnit2
open Matrix
open Rationals
open Num

let cmp_rat = fun x1 x2 -> 
    match RATIONAL.compare x1 x2 with
    | EQ -> true
    | _ -> false
let make_op_test 
    (name: string)
    (op: int*int -> int*int -> int*int)
    (input1: int*int)
    (input2: int*int)
    (expected_output: int*int) : test =
  name >:: (fun _ -> 
      assert_equal (op input1 input2) expected_output ~cmp:cmp_rat)

let rationals_tests =
  let add = RATIONAL.add in
  let mul = RATIONAL.mul in
  let div = RATIONAL.div in
  let sub = RATIONAL.sub in
  
  [
    make_op_test "Add - 0 + 0 = 0" add (0,1) (0,1) (0,1);
    make_op_test "Add - x + 0 = x" add (1,1) (0,10) (1,1);
    make_op_test "Add - fractions add up to 1" add (1, 2) (1,2) (1,1);
    make_op_test "Add - fractions add up to whole number > 1" add (1, 2)
      (3, 2) (2, 1);
    make_op_test "Add - sum fraction in simplest form" add (1, 2) (1, 4) (3, 4);
    make_op_test "Add - sum needs to be simplified" add (3, 8) (3, 8) (3, 4);
    make_op_test "Add - sum a fraction > 1" add (1, 2) (2, 2) (3, 2);
    make_op_test "Add - adding fractions > 1" add (42, 5) (21, 5) (63, 5);
    make_op_test "Add - +/-, sum positive" add (1, 2) (-1, 4) (1, 4);
    make_op_test "Add - +/-, sum negative" add (1,4) (-1,2) (-1,4);
    make_op_test "Add - sum of negatives" add (-1,4) (-1,3) (-7,12);
    make_op_test "Add - negative whole num" add (-1,4) (-3,4) (-1,1);
    make_op_test "Add - negative wohle num < -1" add (-3,2) (-1,2) (-2,1);

    make_op_test "Mul - 0 * 0 = 0" mul (0,1) (0,2) (0,1);
    make_op_test "Mul - x * 0 = 0" mul (1,1) (0,1) (0,1);
    make_op_test "Mul - x * 1 = x" mul (1,1) (3,5) (3,5);
    make_op_test "Mul - fractions" mul (1,2) (1,4) (1,8);
    make_op_test "Mul - fractions > 1" mul (3,2) (5,2) (15,4);
    make_op_test "Mul - whole numbers" mul (4,2) (3,1) (6,1);
    make_op_test "Mul - +/- numerators" mul (-1,2) (1,4) (-1,8);
    make_op_test "Mul - +/- denominators" mul (1,2) (-1,2) (-1,4);
    make_op_test "Mul - -/- -> +" mul (-1,2) (-1,2) (1,4);

    make_op_test "Div - x / x = 1" div (1,2) (1,2) (1,1);
    make_op_test "Div - x / 1 = x" div (1,2) (1,1) (1,2);
    make_op_test "Div - whole #s" div (4,1) (2,1) (2,1);
    make_op_test "Div - +/+ fractions" div (1,4) (1,2) (1,2);
    make_op_test "Div - +/- -> -" div (1,4) (-1,2) (-1,2);
    make_op_test "Div - -/- -> +" div (-1,4) (-1,2) (1,2);

    make_op_test "Sub - x - 0 = x" sub (1,2) (0,1) (1,2);
    make_op_test "Sub - x - y > 0" sub (1,2) (1,4) (1,4);
    make_op_test "Sub - x - y < 0" sub (1,4) (1,2) (-1,4);
    make_op_test "Sub --x - y < 0" sub (-1,4) (1,2) (-3,4);
    make_op_test "Sub - subtracting negative" sub (1,4) (-1,2) (3,4);
  ]

let suite = "test suite for LinAlg" >::: List.flatten [
    rationals_tests;
  ]

let _ = run_test_tt_main suite