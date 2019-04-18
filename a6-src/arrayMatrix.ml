open Matrix
(** A module for performing operations on matricies *)
module MAKE_MATRIX : MATRIX_MAKER = functor (T:NUM) -> struct 
  (* AF: A ['a matrix] is constructed from a list of lists with elements of type
   * ['a]. The size of the matrix is denouted by a int*int pair *)
  (* RI: The length of the rows of the matrix must all be equal and the elements 
   * o the matrix must be numeric *)
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

  (* TODO: what if n>r? that would give index out of bounds error *)
  (** [diagonal n r] is the [n] x [r] diagonal matrix with 
    * [1]'s on its diagonal *)
  let diagonal = fun (n:int) (r:int) -> 
    let m = make n r N.zero [[]] in
    for i = 0 to min (n-1) (r-1) do
      m.(i).(i) <- N.one;
    done; m

  (** [dim m] is the dimensions of the matrix [m] *)
  let dim = fun (m:matrix) ->
    Array.length m, Array.length m.(0)

  (** [set m row col entry] sets [row] [col] of [m]atrix to [entry]*)
  let set = fun (m:matrix) (row:int) (col:int) (entry:N.t)-> 
    m.(row).(col) <- entry; m

  (** [transpose m] is the trasnposed matrix of [m] *)
  let transpose = fun (m:matrix) ->
    let rows,cols = dim m in
    let m' = make cols rows N.zero [[]] in
    Array.iteri (fun i row -> Array.iteri (fun j e -> m'.(j).(i) <- e) row) m;m'

  (** [dot u v] is the dot product of two vectors (1d matricies) 
    * Requires: [u] and [v] have the same height and have width of [1] *)
  let dot = fun (u:matrix) (v:matrix) ->
    let (m,n),(p,r) = dim u, dim v in
    if n != 1 || r != 1 || m != p then raise MatrixError else
      let res = ref T.zero in
      for i = 0 to m-1 do 
        res := N.add !res (N.mul u.(i).(0) v.(i).(0))
      done; !res

  (** [scale c m] is the matrix [m] scaled by constant [c] *)
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
    * x1 < x2 and y1 < y2 *)
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
    if m <> p || n <> r then raise MatrixError else
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
    * Requires: i,j < height of m - 1 *)
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
    * Requires: [i] < height of m - 1 *)
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
    * Requries: i, j < height of m - 1 *)
  let addrows = fun (m:matrix) (i:int) (j:int) (c:value) ->
    let e = fun (i:int) (j:int) (c:value) (n:int) -> 
      let res = (diagonal n n) in
      res.(j).(i) <- c;
      res in
    let (n,_) = dim m in
    mul (e i j c n) m

  (* finds the left most pivot in row i *)
  (** [pivot_col m i] is [Some x] where x is the column of the pivot in row [i] 
    * or [None] if there is not pivot in i
    * Requires: i < height of the matrix - 1 *)
  let pivot_col = fun (m:matrix) (i:int) ->
    let p,r = dim m in
    let x = ref 0 in
    while !x < r && N.compare m.(i).(!x) N.zero = EQ do x := !x + 1 done;
    if !x >= r then None else Some !x

  (** [pivot_row m row col] is Some left most pivot starting from position
    * [row], [col] in [m]. If there is no pivot, the value is None *)
  let pivot_row = fun (m:matrix) (row:int) (col:int) ->
    let p,r = dim m in
    let y = ref row in
    while !y < p && N.compare m.(!y).(col) N.zero = EQ do y := !y + 1 done;
    if !y >= p then None else Some !y

  (** [pivots m] are the positions *)
  let pivots = fun (m:matrix) ->
    let p,r = dim m in
    let i,j = ref 0, ref 0 in
    let memo = ref [] in
    while !i < p do
      while !j < r && N.compare m.(!i).(!j) N.zero = EQ do
        j := !j + 1
      done;
      if !j = r then 
        let () = i := !i + 1 in 
        j := 0
      else 
        let () = memo := (!i,!j)::(!memo) in
        let () = i := !i + 1 in
        j := !j + 1
    done; !memo

  (** [free m pvs] are the positions of the missing pivots *)
  let free = fun (m:matrix) (pvs:(int*int) list) ->
    let res = ref [] in
    List.iter (fun (i,j) ->
      if List.mem (i+1,j+1) pvs |> not then
      res := (i+1,j+1)::(!res)
    ) pvs;!res

  (** [reduce m] is the reduced row echelon matrix formed from [m] *)
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

  (** [augment m1 m2] is the matrix obtained by appending 
      * the columns of [m2] to [m1] 
      * Requires: [m1] and [m2] have the same number of rows*)
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
    * [m] x-vector = 0-vector *)
  let null_space = fun (m:matrix) ->
    let rec ins_zero_row = fun (m:matrix) (row:int) ->
      let p,r = dim m in
      if row = p then ins_zero_row m (row - 2) else
      let m' = Array.make_matrix (p + 1) r N.zero in
      let i = ref 0 in
      while !i <= row do
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
    let red = reduce m in
    let memo = ref [] in
    if equals red (diagonal r r) |> not then 
    let pvs = pivots red in
    let not_pvs = free red pvs in
    let tmp = ref red in
    List.iter (
      fun (i,j) -> 
      print_endline (string_of_int i);
      tmp := ins_zero_row (!tmp) i
    ) not_pvs;
    tmp := partition (0,0) (r-1,r-1) (!tmp);
    for i=0 to r - 1 do
      if N.compare (!tmp).(i).(i) N.zero = EQ then
        let vec = partition (i,0) (i,r - 1) (!tmp) in
        vec.(i).(0) <- N.neg N.one;
        memo := vec::(!memo)
    done; else ();
    (make r 1 N.zero [[]])::(!memo)

  (** [col_space m] is the list of vectors that form the column space of [m] *)
  let col_space = fun (m:matrix) -> 
    let p,r = dim m in
    let pvs = pivots (reduce m) in
    let memo = ref [] in
    let is_pivot_col = fun (j:int) ->
      let is_piv = ref false in 
      List.iter (fun (_,j') -> if j = j' then is_piv := true else ()) pvs; 
      !is_piv in
    for j = 0 to r - 1 do
      if (is_pivot_col j) then 
        memo := (partition (j,0) (j,p - 1) m)::(!memo)
      else ()
    done; !memo 

  (** [solve m v] is the list of vectors that solves the linear equation
    * [m] x-vector = [v]. The first vector is the particular solution to the 
    * equation. Fails if the equation cannot be exactly solved for *)
  let solve = fun (m:matrix) (v:matrix) -> 
    let (p,r), (s,t) = dim m, dim v in
    if t <> 1 then raise MatrixError else
      let aug = augment m v |> reduce in
      let vec = partition (r,0) (r,p-1) aug in
      let pvs = pivots aug in 
      List.iter (fun (_,j) -> if j = r then failwith "No solution" else ()) pvs;
      (vec,(null_space m))

  (** [supp_matrix m i j] is the matrix [m] without values from row [i] or 
    * column [j] *)
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

  (*let supp_matrix_1st_row = fun (m:matrix) (col:int) -> 
    let (rows,cols) = dim m in
    let new_mat = make (rows-1) (rows-1) N.zero [[]] in
    for i = 1 to rows-1 do 
      for j = 0 to rows-1 do 
        if (j < col) then new_mat.(i-1).(j) <- m.(i).(j)
        else if (j > col) then new_mat.(i-1).(j-1) <- m.(i).(j)
      done;
    done;
    new_mat*)

  (** [determinant m] is the determinant of matrix [m] *)
  let rec determinant = fun (m:matrix) -> 
    let (row,col) = dim m in if row<>col || row<1 then raise MatrixError else 
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

  (** [inverse m] is the inverse of matrix [m], 
    * Raises [MatrixError] if the matrix [m] is not square or if the
    * determinant of [m] is zero *)
  let inverse = fun (m:matrix) -> 
    let (rows, cols) = dim m in 
    if rows<>cols || (determinant m)=N.zero then raise MatrixError 
    else
      let augmented = augment m (identity rows) in 
      let reduced = reduce augmented in 
      partition (cols, 0) (2*cols-1, rows-1) reduced

  (** [format fmt m] is the formatted matrix [m] *)
  let format = fun (fmt:Format.formatter) (m:matrix) ->
    Format.fprintf fmt "\n";
    Array.iter (fun row -> 
        Format.fprintf fmt "[";
        Array.iter ( fun e -> Format.fprintf fmt " %a " N.format e ) row;
        Format.fprintf fmt "]\n";
      ) m

  (** [format_solution fmt sol] is the formatted solution to a matrix eq *)
  let format_solution = fun (fmt:Format.formatter) (sol:solution) ->
    Format.fprintf fmt "\n";
    format fmt (fst sol);
    List.iteri (fun i m -> Format.fprintf fmt "t%d*%a" i format m) (snd sol);
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

  (* this is incorrect because it's supposed to divide by the square root of the length,
     which requires irrational numbers *)
  let normalize = fun (v:matrix) -> 
    scale (N.div N.one (dot v v)) v

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

  let qr_fact_r = fun (m:matrix) (q:matrix)-> 
    let (rows, cols) = dim m in
    let r = make rows cols N.zero [[]] in
    for col = 0 to cols-1 do
      for row = 0 to col do
        r.(row).(col) <- dot (column q row) (column m col)
      done;
    done; r

  let qr_fact = fun (m:matrix) ->
    let q = qr_fact_q m in
    let r = qr_fact_r m q in
    (q,r)

  let triangular_enough = fun (m:matrix) -> 
    let (rows, cols) = dim m in 
    let boolean = ref true in 
    for col = 0 to cols-1 do
      for row = col+1 to rows-1 do
        boolean := !boolean && (N.to_float (m.(row).(col)) < 0.005)
      done;
    done; !boolean

  let eigenvalues = fun (m:matrix) -> failwith "TODO"
  let eigenvectors = fun (m:matrix) -> failwith "TODO"
end