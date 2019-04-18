open OUnit2
open Matrix
open Rationals
open ArrayMatrix

let cmp_rat = fun x1 x2 -> 
  match RATIONAL.compare x1 x2 with
  | EQ -> true
  | _ -> false

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

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

let make_add_test
    (name: string)
    (input1: RM.matrix)
    (input2: RM.matrix)
    (expected_output: RM.matrix)
    (raises: bool) =
  name >:: (fun _ ->
      if raises then
        assert_raises RM.MatrixError (fun () -> RM.add input1 input2)
      else
        assert_equal expected_output (RM.add input1 input2))

let make_subtract_test
    (name: string)
    (input1: RM.matrix)
    (input2: RM.matrix)
    (expected_output: RM.matrix)
    (raises: bool) =
  name >:: (fun _ ->
      if raises then
        assert_raises RM.MatrixError (fun () -> RM.subtract input1 input2)
      else
        assert_equal expected_output (RM.subtract input1 input2))

let make_scale_test 
    (name: string)
    (c: RM.value)
    (matrix: RM.matrix)
    (expected_output: RM.matrix) =
  name >:: (fun _ ->
      assert_equal (RM.scale c matrix) expected_output ~cmp: RM.equals)

let make_augment_test
    (name: string)
    (m1: RM.matrix)
    (m2: RM.matrix)
    (expected_output: RM.matrix)
    (ex: bool) =
  name >:: (fun _ ->
      if ex then assert_raises RM.MatrixError (fun () -> RM.augment m1 m2)
      else assert_equal expected_output (RM.augment m1 m2)
    )

let make_reduce_test
    (name: string)
    (matrix: RM.matrix)
    (expected_output: RM.matrix) =
  name >:: (fun _ -> 
      assert_equal (RM.reduce matrix) expected_output ~cmp: RM.equals)

let make_determinant_test
    (name: string)
    (matrix: RM.matrix)
    (expected_output: RM.value)
    (ex: bool) = 
  name >:: (fun _ ->  
      if ex then assert_raises RM.MatrixError (fun () -> RM.determinant matrix)
      else assert_equal (RM.determinant matrix) expected_output ~cmp:cmp_rat)

let make_null_space_test
    (name: string)
    (matrix: RM.matrix)
    (expected_output: RM.matrix list) =
  name >:: (fun _ ->
      assert_equal (RM.null_space matrix) expected_output ~cmp:cmp_set_like_lists)

let make_solve_test
    (name: string)
    (matrix: RM.matrix)
    (vector: RM.matrix)
    (expected_output: RM.matrix list) 
    (ex: bool) =
  name >:: (fun _ ->
      if ex then assert_raises RM.MatrixError (fun () -> RM.solve matrix vector)
      else assert_equal (RM.solve matrix vector) expected_output ~cmp: cmp_set_like_lists)

let make_inverse_test
    (name: string)
    (input: RM.matrix)
    (expected_output: RM.matrix)
    (raises: bool) =
  name >:: (fun _ ->
      if raises then
        assert_raises RM.MatrixError (fun () -> RM.inverse input)
      else
        assert_equal expected_output (RM.inverse input) ~cmp:RM.equals
    )

let make_solve_test 
    (name: string)
    (matrix: RM.matrix)
    (vector: RM.matrix)
    (expected_output: RM.matrix list) =
  name >:: (fun _ ->
      assert_equal expected_output (RM.solve matrix vector))

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

    (*=========== matrix transpose tests ===========*)
    make_transpose_test "transpose sq. diagonal matrix = same matrix" 
      (RM.diagonal 3 3) (RM.diagonal 3 3);
    make_transpose_test "transpose rect. matrix swaps rows/cols"
      (RM.diagonal 2 3) (RM.diagonal 3 2);

    (*=========== dot product tests ===========*)
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
          [Int 1; Int (-2); Int 3]
        ])
      (RM.make 2 3 RATIONAL.zero [
          [Int 11; Int 0; Int 21];
          [Int (-1); Int 13; Int (-9)]
        ]) false;
    make_mul_test "mul 3x3 * 3x3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 2; Int 4; Int 2];
          [Int 1; Int 4; Int 0];
          [Int 2; Int 6; Int 0]
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 0; Int 0];
          [Int 0; Int 1; Int 0];
          [Int 0; Int 0; Int 1]
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int 2; Int 4; Int 2];
          [Frac (1,4); Int 4; Int 0];
          [Int 2; Int 6; Int 0]
        ])
      false;
    make_mul_test "mul 3x2 * 2x2"
      (RM.make 3 2 RATIONAL.zero [
          [Int 5; Int 1];
          [Int 2; Int 2];
          [Int 4; Int 1]
        ])
      (RM.make 2 2 RATIONAL.zero [[]])
      (RM.make 3 2 RATIONAL.zero [[]])
      false;

    (*============= matrix addition tests =============*)
    make_add_test "add 2x2 * 2x2"
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 4];
          [Int 5; Int (-2)]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 7; Int 3];
          [Int 11; Int 9]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 8; Int 7];
          [Int 16; Int 7]
        ])
      false;

    make_add_test "add 2x2 * 2x2"
      (RM.make 2 2 RATIONAL.zero [
          [Int (-3); Int 4];
          [Int 6; Int 13]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 3; Int (-8)];
          [Int (-2); Int 2]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 0; Int (-4)];
          [Int 4; Int 15]
        ])
      false;

    make_add_test "add 3x3 * 3x3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 6; Int 4];
          [Int (-3); Int 8; Int 9];
          [Int (-2); Int 7; Int (-1)]
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int (-2); Int 4; Int (-1)];
          [Int 4; Int (-17); Int (-5)];
          [Int 6; Int 1; Int 8]
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int (-1); Int 10; Int 3];
          [Int 1; Int (-9); Int 4];
          [Int 4; Int 8; Int 7]
        ])
      false;

    make_add_test "add fails with invalid matrix sizes"
      (RM.make 2 3 RATIONAL.one [[]])
      (RM.make 3 3 RATIONAL.one [[]])
      (RM.make 1 1 RATIONAL.error [[]])
      true;

    (*============= matrix subtraction tests =============*)
    make_subtract_test "subtract 2x3 * 2x3"
      (RM.make 2 3 RATIONAL.zero [
          [Int (-1); Int 2; Int 0];
          [Int 0; Int 3; Int 6]
        ])
      (RM.make 2 3 RATIONAL.zero [
          [Int 0; Int (-4); Int 3];
          [Int 9; Int (-4); Int (-3)]
        ])
      (RM.make 2 3 RATIONAL.zero [
          [Int (-1); Int 6; Int (-3)];
          [Int (-9); Int 7; Int 9]
        ])
      false;

    make_subtract_test "subtract 2x2 * 2x2"
      (RM.make 2 2 RATIONAL.zero [
          [Int 2; Int (-1)];
          [Int 1; Int 2]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 3; Int 3];
          [Int 3; Int 1]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int (-1); Int (-4)];
          [Int (-2); Int 1]
        ])
      false;

    make_subtract_test "subtract fails with invalid matrix sizes"
      (RM.make 2 3 RATIONAL.one [[]])
      (RM.make 2 2 RATIONAL.one [[]])
      (RM.make 1 1 RATIONAL.error [[]])
      true;

    (*################## SCALE TEST #################*)
    make_scale_test "basic scaling"
      (Frac (1,2))
      (RM.make 3 4 (Int 5) [[]])
      (RM.make 3 4 (Frac (5,2)) [[]]);

    (*############### AUGMENT TEST ##################*)
    make_augment_test "basic augment"
      (RM.make 3 4 (Int 4) [[]])
      (RM.make 3 4 (Int 4) [[]])
      (RM.make 3 8 (Int 4) [[]])
      false;
    make_augment_test "augment fails"
      (RM.make 2 2 (Int 4) [[]])
      (RM.make 3 3 (Int 2) [[]])
      (RM.make 1 1 RATIONAL.error [[]])
      true;
    (* ################## REDUCE TEST ##############*)
    make_reduce_test "reduce #1"
      (RM.make 3 6 RATIONAL.zero [
          [Int 0;Int 3;Int (-6);Int 6;Int 4;Int (-5)];
          [Int 3;Int (-7);Int 8;Int (-5);Int 8;Int 9];
          [Int 3;Int (-9);Int 12;Int (-9);Int 6;Int 15]])
      (RM.make 3 6 RATIONAL.zero [
          [Int 1; Int 0; Int (-2);Int 3; Int 0;Int (-24)];
          [Int 0; Int 1; Int (-2);Int 2; Int 0;Int (-7)];
          [Int 0; Int 0; Int 0; Int 0; Int 1; Int 4]
        ]);
    make_reduce_test "reduce #2"
      (RM.make 3 6 RATIONAL.zero [
          [Int 1; Int 6; Int 2; Int (-5); Int (-2); Int (-4)];
          [Int 0; Int 0; Int 2; Int (-8); Int (-1); Int 3];
          [Int 0; Int 0; Int 0; Int 0; Int 1; Int 7]
        ])
      (RM.make 3 6 RATIONAL.zero [
          [Int 1; Int 6; Int 0; Int 3; Int 0; Int 0];
          [Int 0; Int 0; Int 1; Int (-4); Int 0; Int 5];
          [Int 0; Int 0; Int 0; Int 0; Int 1; Int 7]
        ]);
    make_reduce_test "reduce #3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 2; Int 7];
          [Int (-2); Int 5; Int 4];
          [Int (-5); Int 6; Int (-3)]
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 0; Int 3];
          [Int 0; Int 1; Int 2];
          [Int 0; Int 0; Int 0]
        ]);

    (*============ determinant tests ==============*)
    make_determinant_test "det - 1x1 matrix"
      (RM.make 1 1 RATIONAL.zero [[Int 1]]) 
      (Int 1)
      false;

    make_determinant_test "det - 2x2 matrix"
      (RM.make 2 2 RATIONAL.zero [
          [Int 4; Int 6];
          [Int 3; Int 8]])
      (Int 14)
      false;

    make_determinant_test "det - 3x3 matrix #1"
      (RM.make 3 3 RATIONAL.zero [
          [Int 0; Int 3; Int 5];
          [Int 5; Int 5; Int 2];
          [Int 3; Int 4; Int 3]])
      (Int (-2))
      false;

    make_determinant_test "det - 3x3 matrix #2"
      (RM.make 3 3 RATIONAL.zero [
          [Int 10; Int 0; Int (-3)];
          [Int (-2); Int (-4); Int 1];
          [Int 3; Int 0; Int 2]])
      (Int (-116))
      false; 

    make_determinant_test "det - 5x5 matrix #2"
      (RM.make 5 5 RATIONAL.zero [
          [Int 6; Int 9; Int 29; Int 38; Int 298];
          [Int 6; Int 2; Int (-29); Int 8; Int 28];
          [Int 56; Int 19; Int 0; Int 338; Int 2];
          [Int 76; Int 5; Int (-4); Int 3; Int 23];
          [Int 69; Int 19; Int 59; Int 28; Int (-46)];
        ])
      (Int (-3865372704))
      false;

    make_determinant_test "det fails - 4x5 matrix #2"
      (RM.make 4 5 RATIONAL.zero [
          [Int 6; Int 9; Int 29; Int 38; Int 298];
          [Int 6; Int 2; Int (-29); Int 8; Int 28];
          [Int 56; Int 19; Int 0; Int 338; Int 2];
          [Int 76; Int 5; Int (-4); Int 3; Int 23]
        ])
      (RATIONAL.error)
      true;

    (*=================== matrix inverse tests ===============*)
    make_inverse_test "inverse - 2x2"
      (RM.make 2 2 RATIONAL.zero [
          [Int 3; Int 2];
          [Int 8; Int 10]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [(Frac (5,7)); (Frac ((-1),7))];
          [(Frac ((-4),7)); (Frac (3,14))]
        ]
      )
      false;

    make_inverse_test "inverse - 3x3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 4; Int (-3); (Frac (1,2))];
          [Int 2; Int 0; Int 14];
          [(Frac ((-1),2)); Int 0; Int 6]
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int 0; (Frac (6,19)); (Frac ((-14),19))];
          [(Frac ((-1),3)); (Frac (97,228)); (Frac ((-55),57))];
          [Int 0; (Frac (1,38)); (Frac(2,19))]
        ])
      false;

    make_inverse_test "inverse fails - 4x5 matrix #2"
      (RM.make 4 5 RATIONAL.zero [
          [Int 6; Int 9; Int 29; Int 38; Int 298];
          [Int 6; Int 2; Int (-29); Int 8; Int 28];
          [Int 56; Int 19; Int 0; Int 338; Int 2];
          [Int 76; Int 5; Int (-4); Int 3; Int 23]
        ])
      (RM.make 1 1 RATIONAL.error [[]])
      true;    

    make_inverse_test "inverse fails - 3x3 linearly dependent"
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 2; Int 3];
          [Int 4; Int 5; Int 6];
          [Int 7; Int 8; Int 9];
        ])
      (RM.make 1 1 RATIONAL.error [[]])
      true;

    make_inverse_test "inverse fails - 5x5 linearly dependent"
      (RM.make 5 5 RATIONAL.zero [
          [Int 1; Int 2; Int 3; Int 4; Int 5];
          [Int 6; Int 7; Int 8; Int 9; Int 10];
          [Int 11; Int 12; Int 13; Int 14; Int 15];
          [Int 16; Int 17; Int 18; Int 19; Int 20];
          [Int 21; Int 22; Int 23; Int 24; Int 25]
        ])
      (RM.make 1 1 RATIONAL.error [[]])
      true;

    (*============= null space tests ===============*)
    make_null_space_test "null space - 3x3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 3; Int 3; Int 0];
          [Int 2; Int 5; Int 1];
          [Int 3; Int 6; Int 1]
        ])
      ([
        RM.make 3 1 RATIONAL.zero [
          [Frac (1,3)];
          [Frac (-1,3)];
          [Int 1]
        ];
        (RM.make 3 1 RATIONAL.zero [
            []
          ])
      ]);

    make_null_space_test "null space 2x3"
      (RM.make 2 3 RATIONAL.zero [
          [Int 3; Int 5; Int 10];
          [Int 4; Int 6; Int 6]
        ])
      ([
        RM.make 3 1 RATIONAL.zero [
          [Int 15];
          [Int (-11)];
          [Int 1]
        ];
        (RM.make 3 1 RATIONAL.zero [
            []
          ])
      ]);

    make_null_space_test "null space 3x3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 4; Int 8];
          [Int 0; Int 2; Int 1];
          [Int 4; Int 10; Int 8]
        ])
      ([
        (RM.make 3 1 RATIONAL.zero [
            []
          ])
      ]);

    (*============= null space tests ===============*)
    (* make_solve_test "solve 3x3"
       (RM.make 3 3 RATIONAL.zero [
           [Frac (4,3); Frac (3,4); Int 2];
           [Int 2; Int 1; Int 3];
           [Int 9; Frac ((-4),3); Int 1]
         ])
       (RM.make 3 1 RATIONAL.zero [[Int 0]; [Int 3]; [Int 0]])
       (RM.make 3 1 RATIONAL.zero [
           [Frac ((-123),25)];
           [Int (-24)];
           [Frac (307,25)]
         ],
        [RM.make 3 1 RATIONAL.zero [
            []
          ]]);

       make_solve_test "solve 5x5"
       (RM.make 5 5 RATIONAL.zero [
           [Int 2; Int 3; Int 3; Int 4; Int 5];
           [Int 2; Int 1; Int 3; Int 3; Int 9];
           [Int 101; Int (-200); Int 3; Int 4; Int 5];
           [Int 34; Int 2; Int 3; Int 2; Int 1];
           [Int 2; Int 2; Int 1; Int 3; Int 2]
         ])
       (RM.make 5 1 RATIONAL.zero [[Int 34]; [Int 56]; [Int 56]; [Int 23]; [Int 21]])
       (RM.make 5 1 RATIONAL.zero [
           [Frac (4937,6861)];
           [Frac (3883,16009)];
           [Frac ((-770639),144081)];
           [Frac (542644,144081)];
           [Frac (945580,144081)]
         ],
        [RM.make 5 1 RATIONAL.zero [
            []
          ]]);*)
  ]

let suite = "test suite for LinAlg" >::: List.flatten [
    rationals_tests;
    matrix_tests;
  ]

let _ = run_test_tt_main suite