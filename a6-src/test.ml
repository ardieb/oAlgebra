open OUnit2
open Matrix
open Rationals
open ArrayMatrix

let cmp_rat = fun x1 x2 -> 
match RATIONAL.compare x1 x2 with
| EQ -> true
| _ -> false
  
let make_op_test 
  (name: string)
  (op: rational -> rational -> rational)
  (input1: rational)
  (input2: rational)
  (expected_output: rational) : test =
name >:: (fun _ -> 
    assert_equal (op 
                    (input1) 
                    (input2)) 
      (expected_output)
      ~cmp:cmp_rat)

module RM = MAKE_MATRIX(RATIONAL)

let make_transpose_test 
  (name: string)
  (input: RM.matrix)
  (expected_output: RM.matrix) =
name >:: (fun _ ->
    assert_equal (RM.transpose input) expected_output ~cmp: RM.equals)

let make_dot_test 
  (name: string)
  (input1: RM.matrix)
  (input2: RM.matrix)
  (expected_output: RM.value)
  (raises: bool) = 
name >:: (fun _ ->
    if raises then
      assert_raises RM.MatrixError (fun () -> RM.dot input1 input2)
    else 
      (assert_equal (RM.dot input1 input2) expected_output ~cmp: cmp_rat))

let make_mul_test
  (name: string)
  (input1: RM.matrix)
  (input2: RM.matrix)
  (expected_output: RM.matrix)
  (raises: bool) =
name >:: (fun _ ->
    if raises then
      assert_raises RM.MatrixError (fun () -> RM.mul input1 input1)
    else
      assert_equal expected_output (RM.mul input1 input2) ~cmp: RM.equals)

let make_scale_test 
  (name: string)
  (c: RM.value)
  (matrix: RM.matrix)
  (expected_output: RM.matrix) =
name >:: (fun _ ->
    assert_equal (RM.scale c matrix) expected_output ~cmp: RM.equals)


let rationals_tests =
let add = RATIONAL.add in
let mul = RATIONAL.mul in
let div = RATIONAL.div in
let sub = RATIONAL.sub in
[
  make_op_test "Add - 0 + 0 = 0" add (Int (0)) (Int (0)) (Int (0));
  make_op_test "Add - x + 0 = x" add (Int (1)) (Frac ((0,10))) (Int (1));
  make_op_test "Add - fractions add up to 1" add (Frac ((1, 2))) (Frac ((1,2))) (Int (1));
  make_op_test "Add - fractions add up to whole number > 1" add (Frac ((1, 2)))
    (Frac ((3, 2))) (Int (2));
  make_op_test "Add - sum fraction in simplest form" add (Frac ((1, 2))) (Frac ((1, 4))) (Frac ((3, 4)));
  make_op_test "Add - sum needs to be simplified" add (Frac ((3, 8))) (Frac ((3, 8))) (Frac ((3, 4)));
  make_op_test "Add - sum a fraction > 1" add (Frac ((1, 2))) (Frac ((2, 2))) (Frac ((3, 2)));
  make_op_test "Add - adding fractions > 1" add (Frac ((42, 5))) (Frac ((21, 5))) (Frac ((63, 5)));
  make_op_test "Add - +/-, sum positive" add (Frac ((1, 2))) (Frac ((-1, 4))) (Frac ((1, 4)));
  make_op_test "Add - +/-, sum negative" add (Frac ((1,4))) (Frac ((-1,2))) (Frac ((-1,4)));
  make_op_test "Add - sum of negatives" add (Frac ((-1,4))) (Frac ((-1,3))) (Frac ((-7,12)));
  make_op_test "Add - negative whole num" add (Frac ((-1,4))) (Frac ((-3,4))) (Int (-1));
  make_op_test "Add - negative wohle num < -1" add (Frac ((-3,2))) (Frac ((-1,2))) (Int (-2));

  make_op_test "Mul - 0 * 0 = 0" mul (Int (0)) (Int (0)) (Int (0));
  make_op_test "Mul - x * 0 = 0" mul (Int (1)) (Int (0)) (Int (0));
  make_op_test "Mul - x * 1 = x" mul (Int (1)) (Frac ((3,5))) (Frac ((3,5)));
  make_op_test "Mul - fractions" mul (Frac ((1,2))) (Frac ((1,4))) (Frac ((1,8)));
  make_op_test "Mul - fractions > 1" mul (Frac ((3,2))) (Frac ((5,2))) (Frac ((15,4)));
  make_op_test "Mul - whole numbers" mul (Frac ((4,2))) (Int (3)) (Int (6));
  make_op_test "Mul - +/- numerators" mul (Frac ((-1,2))) (Frac ((1,4))) (Frac ((-1,8)));
  make_op_test "Mul - +/- denominators" mul (Frac ((1,2))) (Frac ((-1,2))) (Frac ((-1,4)));
  make_op_test "Mul - -/- -> +" mul (Frac ((-1,2))) (Frac ((-1,2))) (Frac ((1,4)));

  make_op_test "Div - x / x = 1" div (Frac ((1,2))) (Frac ((1,2))) (Int (1));
  make_op_test "Div - x / 1 = x" div (Frac ((1,2))) (Int (1)) (Frac ((1,2)));
  make_op_test "Div - whole #s" div (Int (4)) (Int (2)) (Int (2));
  make_op_test "Div - +/+ fractions" div (Frac ((1,4))) (Frac ((1,2))) (Frac ((1,2)));
  make_op_test "Div - +/- -> -" div (Frac ((1,4))) (Frac ((-1,2))) (Frac ((-1,2)));
  make_op_test "Div - -/- -> +" div (Frac ((-1,4))) (Frac ((-1,2))) (Frac ((1,2)));

  make_op_test "Sub - x - 0 = x" sub (Frac ((1,2))) (Int (0)) (Frac ((1,2)));
  make_op_test "Sub - x - y > 0" sub (Frac ((1,2))) (Frac ((1,4))) (Frac ((1,4)));
  make_op_test "Sub - x - y < 0" sub (Frac ((1,4))) (Frac ((1,2))) (Frac ((-1,4)));
  make_op_test "Sub --x - y < 0" sub (Frac ((-1,4))) (Frac ((1,2))) (Frac ((-3,4)));
  make_op_test "Sub - subtracting negative" sub (Frac ((1,4))) (Frac ((-1,2))) (Frac ((3,4)));
]

let matrix_tests = 
[
  make_transpose_test "transpose sq. diagonal matrix = same matrix" 
    (RM.diagonal 3 3) (RM.diagonal 3 3);
  make_transpose_test "transpose rect. matrix swaps rows/cols"
    (RM.diagonal 2 3) (RM.diagonal 3 2);
  make_dot_test "dot product of two valid vectors"
    (RM.make 5 1 RATIONAL.zero [[Frac (1,1)];[Frac (2,1)];[Frac (3,1)];[Frac (4,1)];[Frac (5,1)]]) 
    (RM.make 5 1 RATIONAL.zero [[Frac (5,1)];[Frac (4,1)];[Frac (3,1)];[Frac (2,1)];[Frac (1,1)]])
    (Int (35)) false; 
  make_dot_test "dot product fails with vectors of different lengths"
    (RM.make 6 1 RATIONAL.zero [[]])
    (RM.make 3 1 RATIONAL.zero [[]])
    (Int 0) true;
  make_dot_test "dot product fails with non-vectors"
    (RM.make 2 2 RATIONAL.one [[]])
    (RM.make 2 2 RATIONAL.one [[]])
    (Int 0) true;
  make_mul_test "mul fails with invalid matrix sizes"
    (RM.make 2 3 RATIONAL.one [[]])
    (RM.make 2 3 RATIONAL.one [[]])
    (RM.make 1 1 RATIONAL.one [[]]) true;
  make_mul_test "mul identity on 2x2 matricies"
    (RM.diagonal 2 2)
    (RM.make 2 2 RATIONAL.zero [[Int 5;Int 5];[Int 0;Int 0]])
    (RM.make 2 2 RATIONAL.zero [[Int 5;Int 5];[Int 0;Int 0]])
    false;
  make_mul_test "mul 3x3 matricies"
   (RM.make 3 3 RATIONAL.zero [
    [Int 5;Frac (6,7);Frac (2,3)];
    [Frac (1,3);Frac (4,5);Frac (6,8)];
    [Frac (1,4);Frac (8,8);Frac (5,4)]])
   (RM.make 3 3 RATIONAL.one [[]])
   (RM.make 3 3 RATIONAL.zero [
     [Frac (137,21);Frac (137,21);Frac (137,21)];
     [Frac (113,60);Frac (113,60);Frac (113,60)];
     [Frac (5,2);Frac (5,2);Frac (5,2)]])
    false;
  make_mul_test "mul 2x2 * 2x3"
    (RM.make 2 2 RATIONAL.zero [
      [Int 2;Int 3];
      [Int 1;Int (-5)]
    ])
    (RM.make 2 3 RATIONAL.zero [
      [Int 4; Int 3; Int 6];
      [Int 1; Int (-2); Frac (3,1)]
    ])
    (RM.make 2 3 RATIONAL.zero [
      [Int 11;Int 0;Int 21];
      [Int (-1); Int 13; Int (-9)]
    ]) false;
]

let suite = "test suite for LinAlg" >::: List.flatten [
  rationals_tests;
  matrix_tests;
]

let _ = run_test_tt_main suite