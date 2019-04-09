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

(** [dot u v] is the dot product of two vectors (1d matricies) *)
let dot = fun (u:matrix) (v:matrix) ->
  let (m,n),(p,r) = dim u, dim v in
  if n != 1 || r != 1 || m != p then raise MatrixError else
  let res = ref T.zero in
  for i = 0 to m-1 do 
    res := T.add !res (T.mul u.(i).(0) v.(i).(0))
  done; !res

(** [scale c m] is the matrix [m] scaled by constant [c] *)
let scale = fun (c:v) (m:matrix) -> 
  let p,r = dim m in
  for i = 0 to p-1 do
  for j = 0 to r-1 do
    m.(i).(j) <- N.mul m.(i).(j) c
  done; done; m

(** [partition (x1,y1) (x2,y2) m] is the sub matrix of [n] with rows [y1..y2] 
  * and columns [x1..x2] *)
let partition = fun ((x1,y1):int*int) ((x2,y2):int*int) (m:matrix) -> 
  let p,r = dim m in
  if y1 > p || y2 > p || x1 > r || x2 > r then raise MatrixError else
  let partition = make (y1-y2+1) (x2-x1+1) N.zero [[]] in
  for i = 0 to (y1-y2) do
  for j = 0 to (x1-x2) do
    partition.(i).(j) <- m.(i+y1).(j+x1)
  done; done;
  partition

(** [mul m1 m2] is the product of two matricies [m1] and [m2] *)
let mul = fun (m1:matrix) (m2:matrix) -> 
  let (m,n),(p,r) = dim m1, dim m2 in
  if n != p then raise MatrixError else 
  let res = make m r N.zero [[]] in
  let m1 = transpose m1 in
  for i = 0 to m-1 do
  for j = 0 to r-1 do
    res.(i).(j) <- dot (partition (i,0) (i,n-1) m1) (partition (j,0) (j,p-1) m2)
  done; done; res
  
let add = fun (m1:matrix) (m2:matrix) -> failwith "TODO"
let reduce = fun (m:matrix) -> failwith "TODO"
let augment = fun (m1:matrix) (m2:matrix) -> failwith "TODO"
let inverse = fun (m:matrix) -> failwith "TODO"
let eigenvalues = fun (m:matrix) -> failwith "TODO"
let eigenvectors = fun (m:matrix) -> failwith "TODO"
let solve = fun (m:matrix) (v:matrix) -> failwith "TODO"
end 

