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
  for i = 0 to n do
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
  let u,v = u.(0), v.(0) in 
  let res = ref T.zero in
  for i = 0 to m do 
    res := T.add !res (T.mul u.(i) v.(i))
  done; !res

(** [scale c m] is the matrix [m] scaled by constant [c] *)
let scale = fun (c:v) (m:matrix) -> 
  let p,r = dim m in
  for i = 0 to p do begin
    for j = 0 to r do
      m.(i).(j) <- N.mul m.(i).(j) c
    done;
    end
  done; m

let partition = fun ((x1,y1):int*int) ((x2,y2):int*int) (m:matrix) -> 
  let p,r = dim m in
  if y1 > p || y2 > p || x1 > r || x2 > r then raise MatrixError else
  let partition = make (y1-y2) (x2-x1) N.zero [[]] in
  for i = 0 to (y1-y2) do
    for j = 0 to (x1-x2) do
      partition.(i).(j) <- m.(i+y1).(j+x1)
    done;
  done;
  partition

(** [mul m1 m2] is the product of two matricies [m1] and [m2] *)
let mul = fun (m1:matrix) (m2:matrix) -> 
  let (m,n),(p,r) = dim m1, dim m2 in
  if n != p then raise MatrixError else failwith "TODO"
  
let add = fun (m1:matrix) (m2:matrix) -> failwith "TODO"
let reduce = fun (m:matrix) -> failwith "TODO"
let augment = fun (m1:matrix) (m2:matrix) -> failwith "TODO"
let inverse = fun (m:matrix) -> failwith "TODO"
let eigenvalues = fun (m:matrix) -> failwith "TODO"
let eigenvectors = fun (m:matrix) -> failwith "TODO"
let solve = fun (m:matrix) (v:matrix) -> failwith "TODO"
end 

