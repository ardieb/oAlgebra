open Matrix
(** A module for performing operations on matricies *)
module MAKE_MATRIX : MATRIX_MAKER = functor (T:NUM) -> struct 
  (* AF: A ['a matrix] is constructed from a list of lists with elements of type
   * ['a]. The size of the matrix is denouted by a int*int pair *)
  (* RI: The length of the rows of the matrix must all be equal and the elements of
   * the matrix must be numeric *)
  module N = T
  type v = N.t
  type matrix = v array array
  exception MatrixError

  (** [make rows cols init l] creates a new matrix with [rows] rows and [cols]
    * columns, initialized with the value [init], and filled with values from
    * [l] in by position in the list as a 2d array *)
  let make = fun (rows:int) (cols:int) (init:v) (l:v list list) ->
    let m = Array.make_matrix rows cols init in
    List.iteri (fun i row -> List.iteri (fun j e -> m.(i).(j) <- e) row) l;
    (m : matrix)

  (** [identity n] is the [n] x [n] identity matrix *)
  let identity = fun (n:int) -> 
    let m = make n n N.zero [[]] in
    for i = 0 to n-1 do
      m.(i).(i) <- N.one;
    done; m

  (** [dim m] is the dimensions of the matrix [m] *)
  let dim = fun (m:matrix) ->
    Array.length m, Array.length m.(0)

  (** [transpose m] is the trasnposed matrix of [m] *)
  let transpose = fun (m:matrix) ->
    let rows,cols = dim m in
    let m' = Array.make_matrix cols rows T.zero in
    Array.iteri (fun i row -> Array.iteri (fun j e -> m'.(j).(i) <- e) row) m; m

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
  let scale = fun (c:v) (m:matrix) -> 
    let p,r = dim m in
    for i = 0 to p-1 do
      for j = 0 to r-1 do
        m.(i).(j) <- N.mul m.(i).(j) c
      done; done; m

  (** [partition (x1,y1) (x2,y2) m] is the sub matrix of [n] with rows [y1..y2] 
    * and columns [x1..x2] 
    * Requires: x1 and x2 are less than the width of the matrix
    * y1 and y2 are less than the height of the matrix
    * x1 < x2 and y1 < y2 *)
  let partition = fun ((x1,y1):int*int) ((x2,y2):int*int) (m:matrix) -> 
    let p,r = dim m in
    if y1 > p || y2 > p || x1 > r || x2 > r || x1 > x2 || y1 > y2 
    then raise MatrixError else
      let partition = make (y1-y2+1) (x2-x1+1) N.zero [[]] in
      for i = 0 to (y1-y2) do
        for j = 0 to (x1-x2) do
          partition.(i).(j) <- m.(i+y1).(j+x1)
        done; done;
      partition

  (** [mul m1 m2] is the product of two matricies [m1] and [m2] 
    * Requires: the width of [m1] is equal to the height of [m2] *)
  let mul = fun (m1:matrix) (m2:matrix) -> 
    let (m,n),(p,r) = dim m1, dim m2 in
    if n != p then raise MatrixError else 
      let res = make m r N.zero [[]] in
      let m1 = transpose m1 in
      for i = 0 to m-1 do
        for j = 0 to r-1 do
          res.(i).(j) <- dot (partition (i,0) (i,n-1) m1) (partition (j,0) (j,p-1) m2)
        done; done; res

  (** [add m1 m2] is the sum of matricies [m1] and [m2] 
    * Requires: [m1] and [m2] have the same dimensions *)
  let add = fun (m1:matrix) (m2:matrix) -> 
    let (m,n), (p,r) = dim m1, dim m2 in
    if m != p || n != r then raise MatrixError else
      let res = make m n N.zero [[]] in
      for i = 0 to m-1 do
        for j = 0 to n-1 do
          res.(i).(j) <- N.add m1.(i).(j) m2.(i).(j)
        done; done; res

  let reduce = fun (m:matrix) -> failwith "TODO"
  let augment = fun (m1:matrix) (m2:matrix) -> failwith "TODO"
  let inverse = fun (m:matrix) -> failwith "TODO"
  let eigenvalues = fun (m:matrix) -> failwith "TODO"
  let eigenvectors = fun (m:matrix) -> failwith "TODO"
  let solve = fun (m:matrix) (v:matrix) -> failwith "TODO"

  let supp_matrix_1st_row = fun (m:matrix) (col:int) -> 
    let (rows,cols) = dim m in
    let new_mat = make (rows-1) (rows-1) N.zero [[]] in
    for i = 1 to rows do 
      for j = 0 to rows do 
        if (j < col) then new_mat.(i-1).(j) <- m.(i).(j)
        else if (j > col) then new_mat.(i-1).(j-1) <- m.(i).(j)
      done;
    done;
    new_mat

  let rec determinant = fun (m:matrix) -> 
    let (row,col) = dim m in if row<>col || row<2 then raise MatrixError else 
    if row=2 then let (a,b,c,d) = (m.(0).(0),
                                   m.(0).(1),
                                   m.(1).(0),
                                   m.(1).(1)) in 

      N.sub 
        (N.mul a d)
        (N.mul b c)
    else 

      let sum = ref N.zero in 
      for counter = 0 to row do
        let neg_or_pos = if (counter mod 2)=0 then N.one else N.neg_one in 
        sum := N.add (!sum) 
            (neg_or_pos |> N.mul m.(0).(counter) |> N.mul 
               (determinant (supp_matrix_1st_row m counter)))
      done;
      !sum
end 

