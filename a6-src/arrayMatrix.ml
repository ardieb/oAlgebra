open Matrix
(** A module for performing operations on matricies *)
module MAKE_MATRIX : MATRIX_MAKER = functor (T:NUM) -> struct 
  (* AF: A ['a matrix] is constructed from a array of arrays with elements of 
   * type ['a]. The size of the matrix is denoted by a int*int pair *)
  (* RI: The length of the rows of the matrix must all be equal and the elements 
   * of the matrix must be numeric *)
  module N = T
  type value = N.t
  type matrix = value array array
  type solution = matrix * matrix list
  exception MatrixError

  (** [make rows cols init l] creates a new matrix with [rows] rows and [cols]
    * columns, initialized with the value [init], and filled with values from
    * [l] in by position in the list as a 2d array *)
  let make = fun (rows:int) (cols:int) (init:value) (l:value list list) ->
    let m = Array.make_matrix rows cols init in
    List.iteri (fun i row -> List.iteri (fun j e -> m.(i).(j) <- e) row) l;
    (m : matrix)

  (** [diagonal n r] is the [n] x [r] diagonal matrix with 
    * [1]'s on its diagonal *)
  let diagonal = fun (n:int) (r:int) -> 
    let m = make n r N.zero [[]] in
    for i = 0 to min (n-1) (r-1) do
      m.(i).(i) <- N.one;
    done; m

  (** [dim m] is the dimensions of the matrix [m], in row-column order *)
  let dim = fun (m:matrix) ->
    Array.length m, Array.length m.(0)

  (** [set m row col entry] is the matrix [m] with  [row] [col] set to [entry]*)
  let set = fun (m:matrix) (row:int) (col:int) (entry:N.t)-> 
    m.(row).(col) <- entry; m

  (** [transpose m] is the transposed matrix of [m] *)
  let transpose = fun (m:matrix) ->
    let rows,cols = dim m in
    let m' = make cols rows N.zero [[]] in
    Array.iteri (fun i row -> Array.iteri (fun j e -> m'.(j).(i) <- e) row) m;m'

  (** [dot u v] is the dot product of two vectors (1d matricies) 
    * Requires: [u] and [v] have the same height and have width of [1] 

    * Example: 
    * given vector 1 is 
    * [2]
    * [8]
    * [1]
    * and vector 2 is
    * [8]
    * [9]
    * [8]
    * the result is 
    * 96 *)
  let dot = fun (u:matrix) (v:matrix) ->
    let (m,n),(p,r) = dim u, dim v in
    if n != 1 || r != 1 || m != p then 
      raise MatrixError
    else
      let res = ref T.zero in
      for i = 0 to m-1 do 
        res := N.add !res (N.mul u.(i).(0) v.(i).(0))
      done; !res
  (** [scale c m] is the matrix [m] scaled by constant [c] 
    * Example: 
    * given matrix is 
    * [1; 6; 2]
    * [9; 2; 1]
    * [23; 1; 9]
    * and c is 2
    * then the result is
    * [2; 12; 4]
    * [18; 4; 2]
    * [46; 2; 18] *)
  let scale = fun (c:value) (m:matrix) -> 
    let p,r = dim m in
    let res = make p r N.zero [[]] in
    for i = 0 to p-1 do
      for j = 0 to r-1 do
        res.(i).(j) <- N.mul m.(i).(j) c
      done; done; res
  (** [partition (x1,y1) (x2,y2) m] is the sub matrix of [n] with rows [y1..y2] 
    * and columns [x1..x2] 
    * Requires: x1 and x2 are less than the width of the matrix
    * y1 and y2 are less than the height of the matrix
    * x1 < x2 and y1 < y2 
    * Given (x1,y1) is (1,1) and (x2,y2) is (2,2) and the matrix is 
    * [1; 2; 3]
    * [4; 5; 6]
    * [7; 8; 9]
    * then the result is 
    * [5; 6]
    * [8; 9] *)
  let partition = fun ((x1,y1):int*int) ((x2,y2):int*int) (m:matrix) -> 
    let p,r = dim m in
    if y1 > p || y2 > p || x1 > r || x2 > r || x1 > x2 || y1 > y2 
    then raise MatrixError else
      let partition = make (y2-y1+1) (x2-x1+1) N.zero [[]] in
      for i = y1 to y2 do
        for j = x1 to x2 do
          partition.(i-y1).(j-x1) <- m.(i).(j)
        done; done;
      partition
  (** [mul m1 m2] is the product of two matricies [m1] and [m2] 
    * Requires: the width of [m1] is equal to the height of [m2] *)
  let mul = fun (m1:matrix) (m2:matrix) -> 
    let (m,n),(p,r) = dim m1, dim m2 in
    if n <> p then raise MatrixError else 
      let res = make m r N.zero [[]] in
      let m1 = transpose m1 in
      for i = 0 to m-1 do
        for j = 0 to r-1 do
          let u,v = (partition (i,0) (i,n-1) m1),(partition (j,0) (j,p-1) m2) in
          res.(i).(j) <- dot u v
        done; done; res
  (** [identity n] is an [n] by [n] identity matrix*)
  let identity = fun (n:int) -> 
    let empty = make n n N.zero [[]] in 
    for i = 0 to (n-1) do 
      empty.(i).(i) <- N.one
    done;
    empty 
  (** [add m1 m2] is the sum of matricies [m1] and [m2] 
    * Requires: [m1] and [m2] have the same dimensions *)
  let add = fun (m1:matrix) (m2:matrix) -> 
    let (m,n), (p,r) = dim m1, dim m2 in
    if m <> p || n <> r then 
      raise MatrixError
    else
      let res = make m n N.zero [[]] in
      for i = 0 to m-1 do
        for j = 0 to n-1 do
          res.(i).(j) <- N.add m1.(i).(j) m2.(i).(j)
        done; done; res
  (** [sub m1 m2] is the difference of matricies [m1] and [m2] 
      * Requires: [m1] and [m2] have the same dimensions *)
  let subtract = fun (m1:matrix) (m2:matrix) -> 
    add m1 (scale (N.neg N.one) m2)
  (* HELPERS-FOR-REDUCE *)
  (* matrix for swapping the ith and jth rows *)
  (** [swaprows m i,j] is the matrix [m] with rows [i] and [j] interchanged 
    * Requires: i,j < height of m - 1 
    * ALGORITHM: Create an elementary swap matrix and multiply [m] by [e] *)
  let swaprows = fun (m:matrix) (i:int) (j:int) -> 
    let e = fun (i:int) (j:int) (n:int) ->
      let res = (diagonal n n) in
      res.(i).(i) <- N.zero;
      res.(j).(j) <- N.zero;
      res.(i).(j) <- N.one;
      res.(j).(i) <- N.one;
      res in
    let (n,_) = dim m in
    mul (e i j n) m
  (* matrix for multiplying a row *)
  (** [mulrows m i c] is dthe matrix [m] with row [i] multiplied by [c]
    * Requires: [i] < height of m - 1 
    * ALGORITHM: Create an elementary row scale matrix and multiply [m] by [e]*)
  let mulrows = fun (m:matrix) (i:int) (c:value) ->
    let e = fun (i:int) (c:value) (n:int) ->
      let res = (diagonal n n) in
      res.(i).(i) <- c;
      res in
    let (n,_) = dim m in
    mul (e i c n) m
  (* matrix for adding one row to another *)
  (** [addrows m i,j c] is the matrix [m] with row [i] multiplied by [c] added 
    * to row [j] in [m] 
    * Requries: i, j < height of m - 1 
    * ALGORITHM: Create an elementary row add matrix and multiply [m] by [e] *)
  let addrows = fun (m:matrix) (i:int) (j:int) (c:value) ->
    let e = fun (i:int) (j:int) (c:value) (n:int) -> 
      let res = (diagonal n n) in
      res.(j).(i) <- c;
      res in
    let (n,_) = dim m in
    mul (e i j c n) m
  (* finds the left most pivot in [row] *)
  (** [pivot_col m row] is [Some x] where x is the column of the pivot in row 
      [i]  or [None] if there is not pivot in [row]
    * Requires: [row] < height of the matrix - 1 
    * ALGORITHM: Compare the elements from left to right in [row] to zero
    * and return the column of the first non-zero element *)
  let pivot_col = fun (m:matrix) (row:int) ->
    let p,r = dim m in
    let x = ref 0 in
    while !x < r && (N.compare m.(row).(!x) N.zero = EQ) do
      x := !x + 1 
    done;
    if !x >= r then None else Some !x
  (** [pivot_row m row col] is Some left most pivot starting from position
    * [row], [col] in [m]. If there is no pivot, the value is None
    * ALGORITHM: Compare the elements from [row] to [p] to zero and 
    * return the first non-zero element *)
  let pivot_row = fun (m:matrix) (row:int) (col:int) ->
    let p,r = dim m in
    let y = ref row in
    while !y < p && N.compare m.(!y).(col) N.zero = EQ do y := !y + 1 done;
    if !y >= p then None else Some !y
  (** [pivots m] is a Hashtbl of all indexes in [m] paired with whether they are
    * pivots or not
    * Requires: [m] is a reduced-echecolon form matrix 
    * ALGORITHM: Start from top to bottom, and then from left to right,
    * compare the indexes in m to zero and add to the Hashtbl the indexes that 
    * are the first non-zero occurances in their row *)
  let pivots = fun (m:matrix) ->
    let p,r = dim m in
    let i,j = ref 0, ref 0 in
    let memo = Hashtbl.create (p*r) in
    while !i < p do
      while !j < r && N.compare m.(!i).(!j) N.zero = EQ do
        let () = Hashtbl.add memo (!i,!j) false in 
        j := !j + 1
      done;
      if !j = r then 
        let () = i := !i + 1 in 
        j := 0
      else 
        let () = Hashtbl.add memo (!i,!j) true in
        j := !j + 1;
        while !j < r do
          let () = Hashtbl.add memo (!i,!j) false in 
          j := !j + 1
        done; 
        let () = i := !i + 1 in
        j := 0
    done; memo
  (** [get matrix row col] is the element [row] [col] from [matrix]
    * 
    * Requires: 
    * row and col are within bounds of [matrix]'s dimensions *)
  let get = fun (m:matrix) (r:int) (c:int) ->
    let rows,cols = dim m in 
    if r>=rows || c>=cols then 
      raise MatrixError
    else 
      m.(r).(c)
  (** [reduce m] is the reduced row echelon matrix formed from [m] 
    * ALGORITHM: first reduce [m] to an upper triangular matrix using forward.
    * Then working from the bottom-left pivot up to the top add multiples of the
    * pivot rows to the above rows until no further reductions can be made 
    *
    * given the matrix 
    * [1; 2; 1]
    * [-2; -3; 1]
    * [3; 5; 0]
    *
    * the result is
    * [1; 0; -5]
    * [0; 1; 3]
    * [0; 0; 0] *)
  let reduce = fun (m:matrix) -> 
    let p,r = dim m in
    let memo = ref m in
    (* Gets matrix in upper triangular form *)
    let rec forward (row:int) (col:int) =
      if row >= p - 1 || col >= r - 1 then () else 
        match pivot_row (!memo) row col with
        | None -> forward (row) (col + 1);
        | Some pivot -> begin
            memo := swaprows (!memo) row pivot;
            for i = row + 1 to p - 1 do
              let const = N.neg (N.div (!memo).(i).(col) (!memo).(row).(col)) in
              memo := addrows (!memo) row i const 
            done;
            forward (row + 1) (col + 1);
          end in 
    let rec backward (row:int) =
      if row < 0 then () else
        match pivot_col (!memo) row with
        | None -> 
          backward (row -1);
        | Some col when row = 0 ->
          memo := mulrows !memo row (N.div N.one (!memo).(row).(col))
        | Some col -> begin
            memo := mulrows !memo row (N.div N.one (!memo).(row).(col));
            for i = row - 1 downto 0 do
              memo := addrows !memo row (i) (N.neg (!memo).(i).(col));
            done; let row' = ref row in
            while !row' > 0 && pivot_col (!memo) (!row' - 1) = None do
              memo := swaprows !memo !row' (!row' - 1);
              row' := !row' - 1;
            done; backward (!row' - 1)
          end in 
    forward 0 0; backward (p - 1); !memo

  (** [augment m1 m2] is the matrix obtained by appending the columns of [m2] 
    *  to [m1] 
    * Requires: [m1] and [m2] have the same number of rows 
    *
    * Given m1 is
    * [1; 0; -5]
    * [0; 1; 3]
    * [0; 0; 0]
    *  and m2 is 
    *  [2; 3]
    *  [5; 1]
    *  [7; 0]
    *  then the result is 
    *  [1; 0; -5; 2; 3]
    *  [0; 1; 3; 5; 1]
    *  [0; 0; 0; 7; 0] *)
  let augment = fun (m1:matrix) (m2:matrix) -> 
    let (m,n), (p,r) = dim m1, dim m2 in
    if m != p then raise MatrixError else
      let res = make m (n+r) N.zero [[]] in 
      for i = 0 to m-1 do 
        for j = 0 to (n+r-1) do 
          res.(i).(j) <- if (j < n) then m1.(i).(j)
            else m2.(i).(j-n)
        done; done; res
  (** [equals m1 m2] is true if m1 is structurally equal to m2 and false 
    * otherwise *)
  let equals = fun (m1:matrix) (m2:matrix) ->
    let (m,n), (p,r) = dim m1, dim m2 in
    if m <> p || n <> r then false
    else 
      let res = ref true in
      let i = ref 0 in
      let j = ref 0 in
      while !i < m && !res do
        while !j < n && !res do
          res := N.compare (m1.(!i).(!j)) (m2.(!i).(!j)) = EQ;
          j := !j + 1;
        done;
        i := !i + 1;
      done; !res
  (** [null_space m] is is the list of vectors that solve the equation
    * [m] x-vector = 0-vector 
    * ALGORITHM: Find the pivots in the reduced row echelon form of [m]
    * Then fill in the "gaps" between pivots with rows of zeros and remove any
    * zero-rows from the bottom of the matrix. Then reading along the diagonal 
    * of the reduced and squared matrix, if a zero is encountered, append
    * that column to the null-basis and replace the position in the vector
    * that matches the index of the column with -1 
    *  Given the matrix is 
    *  [-3; 6; -1; 1; -7]
    *  [1; -2; 2; 3; -1]
    *  [2; -4; 5; 8; 4]
    *  The result is 
    *  [2 1 -3]
    *  [1 0  0]
    *  [0 -2 2]
    *  [0 1  0]
    *  [0 0  1] *)
  let null_space = fun (m:matrix) ->
    let ins_row = fun (m:matrix) (row:int) ->
      let p,r = dim m in
      let m' = Array.make_matrix (p + 1) r N.zero in
      let i = ref 0 in
      while !i < row do
        m'.(!i) <- m.(!i);
        i := !i + 1
      done;
      m'.(!i) <- Array.make r N.zero;
      i := !i + 1;
      while !i < p + 1 do
        m'.(!i) <- m.(!i - 1);
        i := !i + 1
      done; m' in
    let p,r = dim m in
    let reduced = reduce m in
    let pvts = pivots reduced in
    let tmp = ref reduced in
    let res = ref [make r 1 N.zero [[]]] in
    let is_piv_col = fun (col:int) -> 
      let res = ref false in
      for i=0 to p - 1 do
        res := Hashtbl.find pvts (i,col) || !res;
      done; !res in
    for j=0 to r - 1 do
      if is_piv_col j |> not then
        tmp := ins_row (!tmp) j
      else ()
    done; 
    tmp := (partition (0,0) (r-1, r-1) (!tmp));
    for j=0 to r - 1 do
      if N.compare (!tmp).(j).(j) N.zero = EQ then
        let vec = (partition (j,0) (j,r - 1) (!tmp)) in
        vec.(j).(0) <- N.neg N.one;
        res := vec::(!res)
      else ()
    done; !res
  (** [col_space m] is the list of vectors that form the column space of [m] 
    * ALGORITHM: Find all the pivots in the reduced echecolon form matrix of [m]
    * and return the pivot columns *)
  let col_space = fun (m:matrix) -> 
    let p,r = dim m in
    let pvts = pivots (reduce m) in
    let memo = ref [] in
    let is_piv_col = fun (col:int) -> 
      let res = ref false in
      for i=0 to p - 1 do
        res := Hashtbl.find pvts (i,col) || !res
      done; !res in
    for j = 0 to r - 1 do
      if (is_piv_col j) then 
        memo := (partition (j,0) (j,p - 1) m)::(!memo)
      else ()
    done; !memo 
  (** [row_space m] is the list of vectors that form the row space of [m], i.e.
    * the rows of [m] that contain pivots. The row shpace is the column space
      of [transpose m]*)
  let row_space = fun (m:matrix) -> 
    m |> transpose |> col_space
  (** [solve m v] is the list of vectors that solves the linear equation
    * [m] x-vector = [v]. The first vector is the particular solution to the 
    * equation. Fails if the equation cannot be exactly solved for
    * ALGORITHM: Augment [m] with [v]. Then reduced the augmented matrix to
    * row echelon form and then read off the pivots of the matrix. If there is 
    * a pivot in the rightmost column, there is no particular solution and fails
    * If there is a solution, the solution is a linear combination of the 
    * null-space of [m] and the particular solution to [m]x = [v], which is the
    * right most column of the reduced-row-echolon augmented matrix *)
  let solve = fun (m:matrix) (v:matrix) -> 
    let (p,r), (s,t) = dim m, dim v in
    if t <> 1 then raise MatrixError else
      let aug = augment m v |> reduce in
      let vec = partition (r,0) (r,p-1) aug in
      let pvts = pivots aug in 
      let is_piv_col = fun (col:int) -> 
        let res = ref false in
        for i=0 to p - 1 do
          res := Hashtbl.find pvts (i,col) || !res
        done; !res in
      if is_piv_col r then raise MatrixError else
        (vec,(null_space m))
  (** [supp_matrix m i j] is the matrix [m] without values from row [i] or 
    * column [j] 
    * ALGORITHM: Add the elements of [m] to a new matrix, excluding elements
    * from [row] or [col] *)
  let supp_matrix = fun (m:matrix) (row:int) (col:int) -> 
    let p,r = dim m in
    let m' = make (p-1) (r-1) N.zero [[]] in
    let i, j = ref 0, ref 0 in
    while !i <> row do 
      while !j <> col do
        m'.(!i).(!j) <- m.(!i).(!j);
        j := !j + 1;
      done;
      while !j < r - 1 do
        m'.(!i).(!j) <- m.(!i).(!j + 1);
        j := !j + 1;
      done;
      i := !i + 1;
      j := 0;
    done;
    while !i < p - 1 do 
      while !j <> col do
        m'.(!i).(!j) <- m.(!i + 1).(!j);
        j := !j + 1;
      done;
      while !j < r - 1 do
        m'.(!i).(!j) <- m.(!i + 1).(!j + 1);
        j := !j + 1;
      done;
      i := !i + 1;
      j := 0;
    done; m'
  (** [determinant m] is the determinant of matrix [m] 
    * ALGORITHM: Cauchy expansion *)
  let rec determinant = fun (m:matrix) -> 
    let (row,col) = dim m in if row<>col || row<1 then 
      raise MatrixError 
    else 
    if row=1 then m.(0).(0) else
    if row=2 then let a,b,c,d = (m.(0).(0), m.(0).(1), m.(1).(0), m.(1).(1)) in 
      N.sub (N.mul a d) (N.mul b c) 
    else 
      let sum = ref N.zero in 
      for counter = 0 to (col-1) do
        let neg_or_pos = if (counter mod 2)=0 then N.one else N.neg N.one in 
        sum := N.add (!sum) 
            (neg_or_pos |> N.mul m.(0).(counter) |> N.mul 
               (determinant (supp_matrix m 0 counter)))
      done; !sum
  (** [inverse m] is the inverse of matrix [m], i.e. the matrix [n] s.t. 
    * [mul m n] = the identity matrix
    * Raises [MatrixError] if the matrix [m] is not square or if the
    * determinant of [m] is zero 
    * ALGORITHM: Augment the identity matrix to [m] and reduce to row echelon 
    * form. The inverse of [m] is read off from the reduced augmented matrix*)
  let inverse = fun (m:matrix) -> 
    let (rows, cols) = dim m in 
    if rows<>cols || (determinant m)=N.zero then 
      raise MatrixError
    else
      let augmented = augment m (identity rows) in 
      let reduced = reduce augmented in 
      partition (cols, 0) (2*cols-1, rows-1) reduced
  (** [format fmt m] is the formatted matrix [m] *)
  let format = fun (fmt:Format.formatter) (m:matrix) ->
    Format.open_vbox 0;
    Format.fprintf fmt "\n";
    Array.iter (fun row -> 
        Format.open_hbox (); 
        Format.fprintf fmt "[";
        Array.iter ( fun e -> Format.fprintf fmt " %a " N.format e ) row;
        Format.fprintf fmt "]\n";
        Format.close_box ()
      ) m;
    Format.close_box ()
  (** [format_solution fmt sol] is the formatted solution to a matrix eq *)
  let format_solution = fun (fmt:Format.formatter) (sol:solution) ->
    format fmt (fst sol);
    List.iteri (fun i m -> Format.fprintf fmt "\n+ t%d x %a\n" i format m) 
      (snd sol);
    Format.fprintf fmt "\n"
  (** [projection v1 v2] is the projection of v1 onto v2*)
  let proj = fun (v1:matrix) (v2:matrix) -> 
    scale (N.div (dot v1 v2) (dot v2 v2)) v2
  (** [col m i] is the [j]th column of matrix m *)
  let column = fun (m:matrix) (j:int) ->
    let (rows, _) = dim m in
    let c = make rows 1 N.zero [[]] in
    for i = 0 to rows-1 do
      c.(i).(0) <- m.(i).(j)
    done; c
  (** [orth_proj basis vector] is the orthogonal projection of [vector] onto 
    * the subspace spanned by the columns of [basis]
    * Raises: MatrixError, if the columns of [b] are linearly dependent  *)
  let orth_proj = fun (b:matrix) (v:matrix) ->
    let rows, cols = dim b in
    let y = ref (make rows 1 N.zero [[]]) in 
    for i = 0 to cols-1 do
      let u = column b i in
      y := add !y  (proj v u)
    done; !y
  (** [distance basis vector] is the distance from [vector] to the subspace 
    * spanned by the columns of [basis] *)
  let distance = fun (b:matrix) (v:matrix) -> 
    N.pow (dot (subtract v (orth_proj b v)) (subtract v (orth_proj b v)))
      (N.make_Float 0.5)

  (** [orth_decomp basis vector] is a tuple containing the orthogonal projection
    * from [vector] to the columns spanned by [basis] and the projection of 
    * [vector] onto the orthogonal subspace of [basis]
    * Raises: MatrixError, if the columns of [b] are linearly dependent *)
  let orth_decomp = fun (b:matrix) (v:matrix) -> 
    let rows, cols = dim b in
    let v_rows,v_cols = dim v in
    let pivs = pivots (reduce b) in
    let pivot_coors = Hashtbl.fold (fun (x,y) boolean acc -> 
      if (Hashtbl.find pivs (x,y)) then y::acc 
      else acc) pivs [] in
    let num_pivs = List.length pivot_coors in
    if num_pivs <> cols || v_cols <> 1 || v_rows <> rows 
    then raise MatrixError else 
      let projection = orth_proj b v
      in 
      let z = subtract v projection in 
      (projection,z)
  (** [magnitude v] is the length of a vector *)
  let magnitude = fun (v:matrix) ->
    N.pow (dot v v) (N.make_Float 0.5)
  (** [normalize v] is the vector [v] scaled to length 1*)
  let normalize = fun (v:matrix) -> 
    scale (N.div N.one (magnitude v)) v
  (** [qr_fact_q m] is an orthogonal matrix such that A = QR, where R is an
    * R is upper triangular*)
  let qr_fact_q = fun (m:matrix) -> 
    let (rows, cols) = dim m in
    (*Base case*)
    let q = ref (make rows 0 N.zero [[]]) in
    for curr_col_ind = 0 to cols-1 do 
      let curr_col = column m curr_col_ind in
      let curr_u = ref (column m curr_col_ind) in
      for curr_num = 0 to curr_col_ind - 1 do 
        curr_u := subtract !curr_u (proj curr_col (column !q curr_num))
      done;
      q := augment !q (normalize !curr_u)
    done; !q
  (** [qr_fact_r m q] is an upper-triangular matrix such that A = QR, where Q is 
    * an orthogonal matrix *)
  let qr_fact_r = fun (m:matrix) (q:matrix)-> 
    let (rows, cols) = dim m in
    let r = make cols cols N.zero [[]] in
    for col = 0 to cols-1 do
      for row = 0 to col do
        r.(row).(col) <- 
          let dot_product = dot (column q row) (column m col) in 
          try (N.float_to_int dot_product) 
          with N.ArithmeticError -> dot_product
      done;
    done; r
  (** [qr_fact m] is the orthogonal matrix [q] and the upper triangular [r] 
    * s.t. m = qr*)
  let qr_fact = fun (m:matrix) ->
    let q = qr_fact_q m in
    let r = qr_fact_r m q in
    (q,r)
  (** [diagonal vals m size] are the values on the diagonal of [m]atrix where 
    * the number of elements on the diagonal is [size] *)
  let diagonal_vals = fun (m:matrix) (size:int)->
    let vals = ref [] in
    for i = 0 to size-1 do 
      vals := m.(i).(i)::(!vals);
    done; !vals
  (** [change_of_basis b1 b2] is the matrix that takes a vector in the basis
    * formed by the columns of [b1] to the basis formed by the columns of [b2]*)
  let change_of_basis = fun (b1:matrix) (b2:matrix) -> 
    let r1,c1 = dim b1 in 
    let r2,c2 = dim b2 in
    if (determinant b1)=N.zero || (determinant b2)=N.zero || r1<>r2 then
      raise MatrixError else
      let augmented = augment b2 b1 in 
      let rref = reduce augmented in 
      partition (c1,0) (c1*2-1,r1-1) rref
  (** [least_square m v] is the particular solution to the least squares 
    * equation [m] x = [v]. This is the vector in the column space of [m] that
    * is closest to the vector [v] *)
  let least_square = fun (m:matrix) (v:matrix) ->
    let (_, matrix_cols) = dim v in 
    let (_, vector_cols) = dim m in
    if vector_cols < 1 || matrix_cols <> 1 then 
      raise MatrixError 
    else mul (inverse (mul (transpose m) m)) (mul (transpose m) v);;
  (** [lu_decomp m] is the LU decomposition of the [m]atrix. L is the lower 
    * triangular matrix of with [1]'s on the diagonal and U is the echelon form
    * matrix such that LU = [m]. This a tupple in which the first value is the
    * lower triangular matrix and the second value is the upper echelon form 
    * matrix *)
  let lu_decomp = fun (m:matrix) ->
    let lu_decomp_helper = fun (m:matrix) -> 
      let rows,cols = dim m in 
      let empty_vec = make rows 1 N.zero [[]] in 
      let vec_array = Array.make rows empty_vec in 
      let u = ref m in 
      let pivs = pivots (reduce m) in
      let curr_idx = ref 0 in
      let pivot_coors = Hashtbl.fold (fun (x,y) boolean acc -> 
        if (Hashtbl.find pivs (x,y)) then y::acc else acc) pivs [] in
      for col = 0 to (cols-1) do 
        if (List.mem col pivot_coors) then 
          (vec_array.(!curr_idx) <- column !u col;
           for i=0 to !curr_idx-1 do 
             vec_array.(!curr_idx).(i).(0) <- N.zero;
           done;
           curr_idx := !curr_idx + 1);
        for row = col+1 to rows-1 do
          match (N.compare N.zero (!u.(row).(col))) with 
          | LT
          | GT ->
            let constant = N.div (N.neg !u.(row).(col)) !u.(col).(col) in 
            u := addrows (!u) col row constant;
          | EQ -> ()
        done;
      done;
      let identity = diagonal rows rows in
      let l = ref (make rows 0 N.zero [[]]) in
      for col = 0 to rows-1 do
        if col>=(List.length pivot_coors) then 
          l := augment !l  (column identity col) else 
          l := augment !l
              (scale (N.div N.one vec_array.(col).(col).(0)) 
                 (Array.get vec_array col)); done; !l,!u in
    try 
      lu_decomp_helper m 
    with 
      N.ArithmeticError -> raise MatrixError
end