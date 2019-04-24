open OUnit2
open Matrix
open Rationals
open ArrayMatrix

(** [cmp_rat x1 x2] is [true] iff x1 equals x2*)
let cmp_rat = fun x1 x2 -> 
  match RATIONAL.compare x1 x2 with
  | EQ -> true
  | _ -> false


(** [cmp_set_like_lists lst1 ls2] is true iff [lst1] and [lst2] contain the same
  * elements, regardless of order or number of repetitions*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [make_<fun>_test arg] builds a test case for the function <fun>, with 
  * arguments defined as needed for the function*)
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

(** [RM] is the matrix module with values of type [RATIONAL] as its entries. *)
module RM = MAKE_MATRIX(RATIONAL)

(** [approx_eq_mat mat1 mat2] is true iff all elements (i,j) of [mat1] are 
  * within 1/10^6 of element (i,j) of [mat2]*)
let approx_eq_mat mat1 mat2 =
  let (r1,c1), (r2,c2) = (RM.dim mat1), (RM.dim mat2) in
  if (r1 != r2) || (c1 != c2) then false
  else
    let eq = ref true in
    for i=0 to (r1-1) do
      for j=0 to (c1-1) do 
        eq := abs_float (RATIONAL.to_float (RATIONAL.sub (RM.get mat1 i j)
                                              (RM.get mat2 i j))) <= 
              (1.0 /. (RATIONAL.tolerance)) && !eq;
      done;
    done; !eq

(** [rat_cmp_to_int x1 x2] is:
  * -1 if RATIONAL.compare x1 x2 evaluates to LT
  *  0 if RATIONAL.compare x1 x2 evaluates to EQ
  *  1 if RATIONAL.compare x1 x2 evaluates to GT *)
let rat_cmp_to_int = fun x1 x2 -> 
  match (RATIONAL.compare x1 x2) with
  | EQ -> 0
  | LT -> (-1)
  | GT -> 1

(** [approx_eq_list list1 list2] is true if all elements i of [list1] are 
  * withim 1/10^6 of element i of [list2] *)
let approx_eq_list list1 list2 = 
  let rec approx_eq_list_help list1 list2 boolean = 
    if List.length list1 <> List.length list2 then false
    else 
      let uniq1 = List.sort rat_cmp_to_int list1 in 
      let uniq2 = List.sort rat_cmp_to_int list2 in 
      match uniq1, uniq2 with 
      | [], [] -> boolean
      | h1::t1, h2::t2 -> 
        let bool_pass = abs_float (RATIONAL.to_float (RATIONAL.sub h1 h2)) <= 
                        (1.0 /. (RATIONAL.tolerance)) in 
        boolean && (approx_eq_list_help t1 t2 bool_pass)
      | _,_-> failwith "should not reach if lists are equal size"
  in approx_eq_list_help list1 list2 true

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
    (matrix: RM.matrix) =
  name >:: (fun _ ->
      let null =
        let res = ref true in
        let p,_ = RM.dim matrix in
        List.iter (fun e ->
            res := RM.equals (RM.mul matrix e) (RM.make p 1 RATIONAL.zero [[]])
          ) (RM.null_space matrix); !res in
      assert_equal true null)

let make_col_space_test
    (name: string)
    (matrix: RM.matrix)
    (expected_output: RM.matrix list) = 
  name >:: (fun _ ->
      assert_equal (RM.col_space matrix) expected_output ~cmp:cmp_set_like_lists)

let make_solve_test
    (name: string)
    (matrix: RM.matrix)
    (vector: RM.matrix)
    (expected_output: RM.solution) 
    (ex: bool) =
  name >:: (fun _ ->
      if ex then assert_raises RM.MatrixError (fun () -> RM.solve matrix vector)
      else assert_equal (RM.solve matrix vector) expected_output ~cmp:
          (fun sol1 sol2 -> RM.equals (fst sol1) (fst sol2)))

let make_inverse_test
    (name: string)
    (input: RM.matrix)
    (expected_output: RM.matrix)
    (raises: bool) =
  name >:: (fun _ ->
      if raises then
        assert_raises RM.MatrixError (fun () -> RM.inverse input)
      else
        assert_equal expected_output (RM.inverse input) ~cmp:RM.equals)

let make_qr_fact_test 
    (name: string)
    (input: RM.matrix)
    (expected_q : RM.matrix)
    (expected_r: RM.matrix) =
  name >:: (fun _ -> 
      assert_equal (fst (RM.qr_fact input)) expected_q ~cmp: approx_eq_mat;
      assert_equal (snd (RM.qr_fact input)) expected_r ~cmp: approx_eq_mat
    )

let make_least_square_test
    (name: string)
    (input_matrix: RM.matrix)
    (input_vector: RM.matrix)
    (expected_output: RM.matrix)
    (raises: bool) = 
  name >:: (fun _ ->
      if raises then
        assert_raises RM.MatrixError (fun () -> RM.least_square input_matrix input_vector)
      else
        assert_equal expected_output (RM.least_square input_matrix input_vector) ~cmp:RM.equals)

let make_orth_decomp_test 
    (name: string)
    (input_basis: RM.matrix)
    (input_vector: RM.matrix)
    (expected_proj : RM.matrix)
    (expected_z: RM.matrix) 
    (raises: bool) =
  name >:: (fun _ -> 
      if raises then (
        assert_raises RM.MatrixError
          (fun () -> 
             RM.orth_decomp input_basis input_vector) 
      )
      else
        (assert_equal (fst (RM.orth_decomp input_basis input_vector)) 
           expected_proj ~cmp: approx_eq_mat;
         assert_equal (snd (RM.orth_decomp input_basis input_vector)) expected_z
           ~cmp: approx_eq_mat);
    )

let make_change_of_basis_test 
    (name: string)
    (input_basis1 : RM.matrix)
    (input_basis2 : RM.matrix)
    (expected : RM.matrix)
    (raises : bool) = 
  name >:: (fun _ -> 
      if raises then (
        assert_raises RM.MatrixError
          (fun () -> RM.change_of_basis input_basis1 input_basis2)
      )
      else
        (assert_equal (RM.change_of_basis input_basis1 input_basis2) expected)
    )

let make_lu_decomp_test 
    (name: string)
    (input: RM.matrix)
    (raises: bool) = 
  name >:: (fun _ ->
      if raises then (
        assert_raises RM.MatrixError
          (fun () -> RM.lu_decomp input)
      )
      else (let l,u = RM.lu_decomp input in 
            (assert_equal (RM.mul l u) input))
    )

let make_solve_test
    (name: string)
    (matrix: RM.matrix)
    (vector: RM.matrix)
    (expected_output: RM.solution) 
    (ex: bool) =
  name >:: (fun _ ->
      if ex then assert_raises RM.MatrixError (fun () -> RM.solve matrix vector)
      else assert_equal (RM.solve matrix vector) expected_output ~cmp:
          (fun sol1 sol2 -> RM.equals (fst sol1) (fst sol2)))

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
      (RM.make 1 1 (Int (-1)) [[]])
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
      (RM.make 1 1 (Int (-1)) [[]])
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
      (RM.make 1 1 (Int (-1)) [[]])
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
      (Int (-1))
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
      (RM.make 1 1 (Int (-1)) [[]])
      true;    

    make_inverse_test "inverse fails - 3x3 linearly dependent"
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 2; Int 3];
          [Int 4; Int 5; Int 6];
          [Int 7; Int 8; Int 9];
        ])
      (RM.make 1 1 (Int (-1)) [[]])
      true;

    make_inverse_test "inverse fails - 5x5 linearly dependent"
      (RM.make 5 5 RATIONAL.zero [
          [Int 1; Int 2; Int 3; Int 4; Int 5];
          [Int 6; Int 7; Int 8; Int 9; Int 10];
          [Int 11; Int 12; Int 13; Int 14; Int 15];
          [Int 16; Int 17; Int 18; Int 19; Int 20];
          [Int 21; Int 22; Int 23; Int 24; Int 25]
        ])
      (RM.make 1 1 (Int (-1)) [[]])
      true;

    (*============= null space tests ===============*)
    make_null_space_test "null space - 3x3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 3; Int 3; Int 0];
          [Int 2; Int 5; Int 1];
          [Int 3; Int 6; Int 1]
        ]);

    make_null_space_test "null space 2x3"
      (RM.make 2 3 RATIONAL.zero [
          [Int 3; Int 5; Int 10];
          [Int 4; Int 6; Int 6]
        ]); 
    make_null_space_test "null space 3x3"
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 4; Int 8];
          [Int 0; Int 2; Int 1];
          [Int 4; Int 10; Int 8]
        ]); 

    (*===================== column space tests ================*)
    make_col_space_test "column space 2x3"
      (RM.make 2 3 RATIONAL.zero [
          [Int 9; Int 2; Int (-3)];
          [Int 92; Int (-23); Int 43]
        ])
      ([(RM.make 2 1 RATIONAL.zero [
           [Int 9];
           [Int 92]
         ])
       ;
         (RM.make 2 1 RATIONAL.zero [
             [Int 2];
             [Int (-23)]
           ])
        ]);

    make_col_space_test "column space 5x3"
      (RM.make 5 3 RATIONAL.zero [
          [Int 19; Int 2; Int (-32)];
          [Int 292; Int (-3); Int 4];
          [Int 22; Int 82; Int 1];
          [Int (-82); Int 0; Int 0];
          [Int 52; Int 91; Int 25];
        ])
      ([(RM.make 5 1 RATIONAL.zero [
           [Int (-32)];
           [Int 4];
           [Int 1];
           [Int 0];
           [Int 25]
         ])
       ;
         (RM.make 5 1 RATIONAL.zero [
             [Int 19];
             [Int 292];
             [Int 22];
             [Int (-82)];
             [Int 52]
           ]);
         (RM.make 5 1 RATIONAL.zero [
             [Int 2];
             [Int (-3)];
             [Int 82];
             [Int 0];
             [Int 91]
           ])
        ]);
    make_col_space_test "column space 4x6"
      (RM.make 4 6 RATIONAL.zero [
          [Int 3; Int 6; Int 81; Int 0; Int 9; Int 7];
          [Int 7; Int 0; Int 0; Int 8; Int 6; Int 2];
          [Int 1; Int 0; Int 8; Int (-13); Int 4; Int 2];
          [Int (-5); Int 92; Int 17; Int 8; Int 9; Int 1]
        ])
      (
        [
          RM.make 4 1 RATIONAL.zero [
            [Int 0];
            [Int 8];
            [Int (-13)];
            [Int 8];
          ];
          RM.make 4 1 RATIONAL.zero [
            [Int 81];
            [Int 0];
            [Int 8];
            [Int 17];
          ];
          RM.make 4 1 RATIONAL.zero [
            [Int 3];
            [Int 7];
            [Int 1];
            [Int (-5)];
          ];
          RM.make 4 1 RATIONAL.zero [
            [Int 6];
            [Int 0];
            [Int 0];
            [Int 92];
          ]  
        ]
      );

    (*============= solve tests ===============*)
    make_solve_test "solve 3x3"
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
         ]])
      false;

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
         ]])
      false;


    (*================== QR Factorization tests ====================*)
    make_qr_fact_test "4 x 3 matrix"
      (RM.make 4 3 RATIONAL.zero [
          [Int (-1); Int (-1); Int 1];
          [Int 1; Int 3; Int 3];
          [Int (-1); Int (-1); Int 5];
          [Int 1; Int 3; Int 7]
        ])
      (RM.make 4 3 RATIONAL.zero [
          [Frac (-1,2); Frac (1,2); Frac (-1,2)];
          [Frac (1,2); Frac (1,2); Frac (-1,2)];
          [Frac (-1,2); Frac (1,2); Frac (1,2)];
          [Frac (1,2); Frac (1,2); Frac (1,2)];
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int 2; Int 4; Int 2];
          [Int 0; Int 2; Int 8];
          [Int 0; Int 0; Int 4]
        ]);

    make_least_square_test "3 x 2 matrix"
      (RM.make 3 2 RATIONAL.zero 
         [[Int 1; Int 3]; 
          [Int 2; Int 4;]; 
          [Int 1; Int 6;]
         ])
      (RM.make 3 1 RATIONAL.zero 
         [[Int 4;]; 
          [Int 1;]; 
          [Int 3;]
         ])
      (RM.make 2 1 RATIONAL.zero 
         [[Frac ((-29), 77);]; [
             Frac (51, 77);]]
      )
      (false);

    make_least_square_test "3 x 2 matrix"
      (RM.make 3 2 RATIONAL.zero
         [[Int 1; Int 3]; 
          [Int 2; Int 4;]; 
          [Int 1; Int 6;]
         ])
      (RM.make 3 2 RATIONAL.zero 
         [[Int 4; Int 4;]; 
          [Int 1; Int 1;];
          [Int 3; Int 3;]
         ])
      (RM.make 2 1 RATIONAL.zero 
         [[Frac ((-29), 77);]; [
             Frac (51, 77);]]
      )
      (true);

    make_qr_fact_test "3 x 3 matrix"
      (RM.make 3 3 RATIONAL.zero [
          [Int 12; Int (-51); Int 4];
          [Int 6; Int 167; Int (-68)];
          [Int (-4); Int 24; Int (-41)];
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Frac (6,7); Frac (-69,175); Frac (-58,175)];
          [Frac (3,7); Frac (158,175); Frac (6,175)];
          [Frac (-2,7); Frac (6,35); Frac (-33,35)];
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int 14; Int 21; Int (-14)];
          [Int 0; Int 175; Int (-70)];
          [Int 0; Int 0; Int 35]
        ]);

    (*================ orthogonal decomposition ===================*)
    make_orth_decomp_test "3x2 basis, R3 vector"
      (RM.make 3 2 RATIONAL.zero [
          [Int 2; Int (-2)];
          [Int 5; Int 1];
          [Int (-1); Int 1]
        ])
      (RM.make 3 1 RATIONAL.zero [
          [Int 1];
          [Int 2];
          [Int 3];
        ])
      (RM.make 3 1 RATIONAL.zero [
          [Frac ((-2),5)];
          [Int 2];
          [Frac (1,5)]
        ])
      (RM.make 3 1 RATIONAL.zero [
          [Frac (7,5)];
          [Int 0];
          [Frac (14,5)]
        ]) false;

    make_orth_decomp_test "3x2 basis, R3 vector #2"
      (RM.make 3 2 RATIONAL.zero [
          [Int 1; Int (-1)];
          [Int 1; Int 1];
          [Int 0; Int 0];
        ])
      (RM.make 3 1 RATIONAL.zero [
          [Int (-1)];
          [Int 4];
          [Int 3]
        ])
      (RM.make 3 1 RATIONAL.zero [
          [Int (-1)];
          [Int 4];
          [Int 0]
        ])
      (RM.make 3 1 RATIONAL.zero [
          [Int 0];
          [Int 0];
          [Int 3]
        ]) false;

    make_orth_decomp_test "4x3 basis, R4 vector"
      (RM.make 4 3 RATIONAL.zero [
          [Int 1; Int (-1); Int (-1)];
          [Int 1; Int 3; Int 0];
          [Int 0; Int 1; Int 1];
          [ Int 1; Int (-2); Int 1]
        ])
      (RM.make 4 1 RATIONAL.zero [
          [Int 4];
          [Int 3];
          [Int 3];
          [Int (-1)]
        ])
      (RM.make 4 1 RATIONAL.zero [
          [Int 2];
          [Int 4];
          [Int 0];
          [Int 0];
        ])
      (RM.make 4 1 RATIONAL.zero [
          [Int 2];
          [Int (-1)];
          [Int 3];
          [Int (-1)];
        ]) false;

    make_orth_decomp_test "fails with linear dependence"
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 2];
          [Int 2; Int 4]
        ])
      (RM.make 2 1 RATIONAL.zero [
          [Int 1];
          [Int 1]
        ])
      (RM.make 0 0 RATIONAL.zero [[]])
      (RM.make 0 0 RATIONAL.zero [[]])
      true;

    make_orth_decomp_test "fails with vector having more than 1 column (not a
    fucking vector) "
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 2];
          [Int 3; Int 4]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 2];
          [Int 1; Int (-2)]
        ])
      (RM.make 0 0 RATIONAL.zero [[]])
      (RM.make 0 0 RATIONAL.zero [[]])
      true;

    make_orth_decomp_test "fails with number of rows not lining up between the
    basis and the vector "
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 2];
          [Int 3; Int 4]
        ])
      (RM.make 1 1 RATIONAL.zero [
          [Int 1];
        ])
      (RM.make 0 0 RATIONAL.zero [[]])
      (RM.make 0 0 RATIONAL.zero [[]])
      true;

    (*======================== change of basis tests ====================== *)
    make_change_of_basis_test "2x2 change of basis test"
      (RM.make 2 2 RATIONAL.zero [
          [Int (-9); Int (-5)];
          [Int 1; Int (-1)]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 3];
          [Int (-4); Int (-5)]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int 6; Int 4];
          [Int (-5); Int (-3)]
        ])
      false;

    make_change_of_basis_test "2x2 change of basis test"
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 3];
          [Int (-4); Int (-5)]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int (-9); Int (-5)];
          [Int 1; Int (-1)]
        ])
      (RM.inverse (RM.make 2 2 RATIONAL.zero [
           [Int 6; Int 4];
           [Int (-5); Int (-3)]
         ]))
      false;

    make_change_of_basis_test "2x2 change of basis fails because matrix1
      is not a basis"
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 3];
          [Int 2; Int 6]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int (-9); Int (-5)];
          [Int 1; Int (-1)]
        ])
      (RM.make 2 2 RATIONAL.zero [[]])
      true;

    make_change_of_basis_test "2x2 change of basis fails because matrix2
      is not a basis"
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 3];
          [Int 2; Int 5]
        ])
      (RM.make 2 2 RATIONAL.zero [
          [Int (-9); Int (-5)];
          [Int 9; Int 5]
        ])
      (RM.make 2 2 RATIONAL.zero [[]])
      true;

    make_change_of_basis_test "2x2 change of basis fails because matrix1 and
      matrix2 are different sizes"
      (RM.make 2 2 RATIONAL.zero [
          [Int 1; Int 3];
          [Int 2; Int 5]
        ])
      (RM.make 3 3 RATIONAL.zero [
          [Int 1; Int 0; Int 0];
          [Int 0; Int 1; Int 0];
          [Int 0; Int 0; Int 1]
        ])
      (RM.make 2 2 RATIONAL.zero [[]])
      true;

    (*====================== LU decomposition tests ====================*)
    make_lu_decomp_test "4x4 LU decomposition"
      (RM.make 4 4 RATIONAL.zero [
          [Int 3; Int (-7); Int (-2); Int 2];
          [Int (-3); Int 5; Int 1; Int 0];
          [Int 6; Int (-4); Int 0; Int (-5)];
          [Int (-9); Int 5; Int (-5); Int 12]
        ])
      false;

    make_lu_decomp_test "3x4 LU decomposition"
      (RM.make 3 4 RATIONAL.zero [
          [Int 2; Int (-4); Int 4; Int (-2)];
          [Int 6; Int (-9); Int 7; Int (-3)];
          [Int (-1); Int (-4); Int 8; Int 0]
        ])
      false;

    make_lu_decomp_test "5x3 LU decomposition"
      (RM.make 5 3 RATIONAL.zero [
          [Int 2; Int (-6); Int 6];
          [Int (-4); Int 5; Int (-7)];
          [Int 3; Int 5; Int (-1)];
          [Int (-6); Int 4; Int (-8)];
          [Int 8; Int (-3); Int 9];
        ])
      false;

    make_lu_decomp_test "LU decomposition fails"
      (RM.make 2 2 RATIONAL.zero [
          [Int 0; Int 1];
          [Int 1; Int 1]
        ])
      true;
  ]

let suite = "test suite for LinAlg" >::: List.flatten [
    rationals_tests;
    matrix_tests;
  ]

let _ = run_test_tt_main suite