open Matrix
(** A module for performing operations on matricies *)
module MAKE_MATRIX : MATRIX_MAKER = functor (T:NUM) -> struct 
(* AF: A ['a matrix] is constructed from a list of lists with elements of type
 * ['a]. The size of the matrix is denouted by a int*int pair *)
(* RI: The length of the rows of the matrix must all be equal and the elements of
 * the matrix must be numeric *)
module N = T
type value = N.t
type matrix = value array array
exception MatrixError

(** [make rows cols init l] creates a new matrix with [rows] rows and [cols]
  * columns, initialized with the value [init], and filled with values from
  * [l] in by position in the list as a 2d array *)
let make = fun (rows:int) (cols:int) (init:value) (l:value list list) ->
  let m = Array.make_matrix rows cols init in
  List.iteri (fun i row -> List.iteri (fun j e -> m.(i).(j) <- e) row) l;
  (m : matrix)

(** [diagonal n r] is the [n] x [r] diagonal matrix with [1]'s on its diagonal *)
let diagonal = fun (n:int) (r:int) -> 
  let m = make n r N.zero [[]] in
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


(* HELPERS-FOR-REDUCE *)
(* matrix for swapping the ith and jth rows *)
let swaprows = fun (m:matrix) (i,j:int*int) -> 
  let e = fun (i:int) (j:int) (p,r:int*int) ->
  let res = (diagonal p r) in
  res.(i).(i) <- N.zero;
  res.(j).(j) <- N.zero;
  res.(i).(j) <- N.one;
  res.(j).(i) <- N.one;
  res in
  mul (e i j (dim m)) m

(* matrix for multiplying a row *)
let mulrows = fun (m:matrix) (i:int) (c:value) ->
  let e = fun (i:int) (c:value) (p,r:int*int) ->
  let res = (diagonal p r) in
  res.(i).(i) <- c;
  res in
  mul (e i c (dim m)) m

(* matrix for adding one row to another *)
let addrows = fun (m:matrix) (i,j:int*int) (c:value) ->
  let e = fun (i:int) (j:int) (c:value) (p,r:int*int) -> 
  let res = (diagonal p r) in
  res.(i).(j) <- c;
  res in
  mul (e i j c (dim m)) m

(* finds the left most pivot in row i *)
let pivot = fun (m:matrix) (i:int) ->
  let p,r = dim m in
  let x = ref (r-1) in
  while N.compare m.(i).(!x) N.zero = EQ && !x > -1 do
    x := !x - 1;
  done;
  if !x < 0 then None else Some !x

let reduce = fun (m:matrix) -> 
  let p,r = dim m in
  let memo = ref m in
  (* Gets matrix in upper triangular form *)
  let rec forward = fun (i:int) ->
    if i >= p - 1 then () else
    match pivot (!memo) i with
    | Some x -> begin (* Reduces all values below the pivot to zero *)
      for y=i+1 to p - 1 do
        let c = N.neg (N.div (!memo).(y).(x) (!memo).(i).(x)) in 
        memo := addrows (!memo) (i,y) c
      done; forward (i + 1)
    end
    | None -> (* Swaps this row with the row below it and tries forward again *)
      if i = p - 1 then () else
      memo := swaprows (!memo) (i,i+1);
      forward i in
  let rec backward = fun (i:int) ->
    if i <= 0 then () else 
    match pivot (!memo) i with
    | Some x -> begin
      for y=i-1 downto 0 do
      let c = N.neg ((N.div) (!memo).(y).(x) (!memo).(i).(x)) in
      memo := addrows (!memo) (i,y) c
    done; backward (i - 1)
    end
    | None -> backward (i - 1) in
  forward 0; backward (p - 1); !memo

let supp_matrix_1st_row = fun (m:matrix) (col:int) -> 
  let (rows,cols) = dim m in
  let new_mat = make (rows-1) (rows-1) N.zero [[]] in
  for i = 1 to (rows-1) do 
    for j = 0 to (rows-1) do 
      if (j < col) then new_mat.(i-1).(j) <- m.(i).(j)
      else if (j > col) then new_mat.(i-1).(j-1) <- m.(i).(j)
    done; done; new_mat

let rec determinant = fun (m:matrix) -> 
  let (row,col) = dim m in if row<>col || row<2 then raise MatrixError else 
  if row=2 then let (a,b,c,d) = 
    (m.(0).(0),
      m.(0).(1),
      m.(1).(0),
      m.(1).(1)) in 
    N.sub (N.mul a d) (N.mul b c) else 
    let sum = ref N.zero in 
    for counter = 0 to (row-1) do
      let neg_or_pos = if (counter mod 2)=0 then N.one else N.neg N.one in 
      sum := N.add (!sum) 
          (neg_or_pos |> N.mul m.(0).(counter) |> N.mul 
              (determinant (supp_matrix_1st_row m counter)))
    done; !sum
    
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
let inverse = fun (m:matrix) -> failwith "TODO"
let eigenvalues = fun (m:matrix) -> failwith "TODO"
let eigenvectors = fun (m:matrix) -> failwith "TODO"
let solve = fun (m:matrix) (v:matrix) -> failwith "TODO"
end 
