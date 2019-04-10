open OUnit2
open Matrix
open Rationals
open Num

let make_op_test 
    (name: string)
    (op: int*int -> int*int -> int*int)
    (input1: int*int)
    (input2: int*int)
    (expected_output: int*int) : test =
  name >:: (fun _ -> 
      assert_equal (op input1 input2) expected_output)


let rationals_tests =
  let add = RATIONAL.add in
  [
    make_op_test "Add - 0 + 0 = 0" add (0,1) (0,1) (0,1);
    make_op_test "Add - fractions add up to 1" add (1, 2) (1,2) (1,1);
    make_op_test "Add - fractions add up to whole number > 1" add (1, 2)
      (3, 2) (2, 1);
    make_op_test "Add - sum fraction in simplest form" add (1, 2) (1, 4) (3, 4);
    make_op_test "Add - sum needs to be simplified" add (3, 8) (3, 8) (3, 4);
    make_op_test "Add - sum a fraction > 1" add (1, 2) (2, 2) (3, 2);
    make_op_test "Add - adding fractions > 1" add (42, 5) (21, 5) (63, 5);
    make_op_test "Add - +/-, sum positive" add (1, 2) (-1, 4) (1, 4);
    make_op_test "Add - +/-, sum negative" add (1,4) (-1,2) (1,-4);
    make_op_test "Add - sum of negatives" add (-1,4) (-1,3) (-7,12)
  ]

let suite = "test suite for LinAlg" >::: List.flatten [
    rationals_tests;
  ]

let _ = run_test_tt_main suite