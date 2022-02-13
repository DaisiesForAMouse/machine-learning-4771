open Format
open Lacaml.D
open Bigarray

let points = 10000
let dims = 784

(* returns floats since everything else is in floats to interface with LAPACK *)
let range start n =
  List.init n (fun x -> x + start)

let print_outcome outcome =
  print_string (fst outcome); print_string ","; print_float (snd outcome); print_newline (); flush stdout

(* create a random permutation of 1 to n *)
let center matrix =
  let copy = Mat.create (Array2.dim1 matrix) (Array2.dim2 matrix) in
  Array2.blit matrix copy;
  let rec sum vec index acc = if index = Array1.dim vec + 1 then acc
    else sum vec (index + 1) (acc +. (Array1.get vec index)) in
  let average vec =
    let mean = (sum vec 1 0.) /. (float_of_int (Array1.dim vec)) in
    List.iter (fun n -> (Array1.set vec n ((Array1.get vec n) -. mean))) (range 1 (Array1.dim vec)) in
  List.iter (fun col -> average (Mat.col copy col)) (range 1 (Array2.dim2 copy)); copy

(* pseudo-inverse; only works on square matrices, but the covariance matrix is square *)
(* so thats all that we need *)
let pinv matrix =
  let copy = Mat.create (Array2.dim1 matrix) (Array2.dim2 matrix) in
  Array2.blit matrix copy;
  let (sigma, u, vt) = gesvd copy in
  let sigma_psinv = Mat.of_diag (Vec.map (fun x -> if abs_float x < 0.001 then 0. else (1. /. x)) sigma) in
  gemm (gemm ~transa:`T vt sigma_psinv) ~transb:`T u


let pca matrix dims =
  let bottom_pad matrix size =
    let fill i j = if i <= Array2.dim1 matrix && j <= Array2.dim2 matrix then (Array2.get matrix i j) else 0. in
    Array2.init Bigarray.float64 Bigarray.fortran_layout size (Array2.dim2 matrix) fill in
  let centered = center matrix in
  let trans = Mat.transpose_copy centered in
  let (sigma, u, _) = gesvd trans in
  let score = gemm u (bottom_pad (Mat.of_diag sigma) (Array2.dim2 u)) in
  Mat.transpose_copy (Array2.sub_right score 1 dims)

(* data input; this is slower than in corresponding programs b.c. I'm reading from an exported *)
(* text file, not binary. *)
let read_labels fname size =
  let f = open_in fname in
  let try_read () =
    try Some (input_line f) with End_of_file -> None in
  let rec loop acc row = match (try_read (), row) with
    | (Some s, n) -> Array1.set acc n (float_of_string s); loop acc (n + 1)
    | (None, _) -> close_in f; acc in
  loop (Vec.create size) 1

(* for internal layout reasons we use data points as columns for Fortran (FORTRAN?) compatability *)
let read_digits fname cols rows =
  let insert matrix col_vec col =
    List.iter2 (fun value -> fun row -> Array2.set matrix row col value)
      col_vec (range 1 rows) in
  let f = open_in fname in
  let try_read () =
    try Some (input_line f) with End_of_file -> None in
  let rec loop acc row = match (try_read (), row) with
    | (Some s, n) -> insert acc (List.map float_of_string (Str.split (Str.regexp ",") s)) n; loop acc (n + 1)
    | (None, _) -> close_in f; acc in loop (Mat.create rows cols) 1

(* -1 is the L-infty norm, and p is the L-p norm *)
let lp_norm p v1 v2 =
  let diff = Vec.map abs_float (Vec.sub v1 v2) in
  match p with
  | -1 -> Vec.max diff
  | p -> Vec.sum (Vec.map (fun x -> x ** (float_of_int p)) diff)

(* brute forced knn *)
let knn n p data labels point =
  let dists = Array.init (Array2.dim2 data)
      (fun x -> (x + 1, lp_norm p (Array2.slice_right data (x + 1)) point)) in
  Array.sort (fun x -> fun y -> compare (snd x) (snd y)) dists;
  let nearest = Array.sub dists 0 n in
  let neighbors = Array.map (fun x -> Array1.get labels (fst x)) nearest in
  let rec mode arr seen_arr curr_most index = if index < Array.length arr then
      let n = int_of_float (Array.get arr index) in
      Array.set seen_arr n (Array.get seen_arr n + 1);
      let next_most = if Array.get seen_arr n > (Array.get seen_arr curr_most) then n else curr_most in
      mode arr seen_arr next_most (index + 1)
    else float_of_int curr_most in
  mode neighbors (Array.make 10 0) 0 0

let knn_classifier n p data labels =
  knn n p data labels

let est_mean data =
  let mean = (Array1.create Bigarray.float64 Bigarray.fortran_layout (Array2.dim1 data)) in
  Vec.fill mean 0.;
  let rec average row index acc = if index = Array2.dim2 data + 1
    then acc /. (float_of_int (Array2.dim2 data))
    else average row (index + 1) (acc +. (Array2.get data row index)) in
  List.iter (fun row -> Array1.set mean row (average row 1 0.)) (range 1 (Array2.dim1 data)); mean

let est_cov data mean =
  let dim = Array2.dim1 data in
  let ret = Mat.create dim dim in
  Mat.fill ret 0.;
  let outer_prod v matrix = ger v v matrix in
  List.iter (fun n -> outer_prod (Vec.sub (Mat.col data n) mean) ret)
    (range 1 (Array2.dim2 data)); Mat.scal (1. /. (float_of_int dim)) ret; ret

(* this computes the only part of the mv gaussian density that varies with the input *)
(* you want to MINIMIZE this to get the maximal likelihood *)
let gaussian_exp mean cov_inv input =
  let dist = Vec.sub input mean in
  dot dist (gemv cov_inv dist)

(* this is P[input = d] on the dataset of only labels *)
let cond_prob data labels label =
  let rec filter_data acc index = if index = (Array2.dim2 data) + 1
    then acc else if Array1.get labels index = label
    then filter_data ((Array2.slice_right data index) :: acc) (index + 1)
    else filter_data acc (index + 1) in
  let cond_data = Mat.of_col_vecs_list (filter_data [] 1) in
  let cond_mean = est_mean cond_data in
  let cond_cov_inv = pinv (est_cov cond_data cond_mean) in
  gaussian_exp cond_mean cond_cov_inv

let prob_classifier data labels =
  let cond_probs = List.map (fun n -> cond_prob data labels (float_of_int n)) (range 0 10) in
  let least lst =
    let rec tail_rec_least lst n min_elt = match lst with
      | [] -> snd min_elt
      | _ -> tail_rec_least (List.tl lst) (n +. 1.)
               (if List.hd lst < fst min_elt then (List.hd lst, n) else min_elt) in
    tail_rec_least lst 0. (infinity, -1.) in
  (fun point -> least (List.map (fun f -> f point) cond_probs))

(* split everything into a random partition of training and test data *)
let random_part_data data labels size =
  let permute n =
    let rand_index = List.map (fun c -> (Random.bits (), c)) (range 1 n) in
    let new_perm = List.sort compare rand_index in
    List.map snd new_perm in
  let perm = permute (Array2.dim2 data) in
  let rec fill indices n training_cols test_cols training_labels test_labels = match indices with
    | [] -> (List.rev training_cols, List.rev test_cols,
             List.rev training_labels, List.rev test_labels)
    | _ -> if n > size
      then fill (List.tl indices) (n + 1)
          training_cols (Array2.slice_right data (List.nth perm (n - 1)) :: test_cols)
          training_labels ((Array1.get labels (List.nth perm (n - 1))) :: test_labels)
      else fill (List.tl indices) (n + 1)
          (Array2.slice_right data (List.nth perm (n - 1)) :: training_cols) test_cols
          ((Array1.get labels (List.nth perm (n - 1))) :: training_labels) test_labels in
  let (training, test, training_labels, test_labels) = fill perm 1 [] [] [] [] in
  (Mat.of_col_vecs_list training, Mat.of_col_vecs_list test,
   Vec.of_list training_labels, Vec.of_list test_labels)

let run_test f test_data test_labels =
  let dim = Array2.dim2 test_data in
  let guesses = List.init dim (fun n -> f (Array2.slice_right test_data (n + 1))) in
  let correct = List.length (List.filteri (fun n -> fun guess ->
      guess = (Array1.get test_labels (n + 1))) guesses) in
  (float_of_int correct) /. (float_of_int dim)

let run_random_test knn_params data labels size =
  let (trd, td, trl, tl) = random_part_data data labels size in
  let prob_c = prob_classifier trd trl in
  let outcome_prob = ((String.concat "" ["prob,"; string_of_int size]), run_test prob_c td tl) in
  let gen_c params =
    (String.concat "" ["knn|k="; string_of_int (fst params);
                       "|norm="; string_of_int (snd params);
                       ",";string_of_int size;],
     knn_classifier (fst params) (snd params) trd trl) in
  let knn_cs = List.map (fun params -> gen_c params) knn_params in
  let outcomes_knn = List.map (fun f -> (fst f, run_test (snd f) td tl)) knn_cs in
  outcome_prob :: outcomes_knn

let run_seq_test knn_params data labels size trials =
  let rec tail_rec_run trials acc = match trials with
    | 0 -> acc
    | _ -> tail_rec_run (trials - 1) ((run_random_test knn_params data labels size) :: acc) in
  List.flatten (tail_rec_run trials [])

(* to generate data, do parallel dune exec ./hw1.exe ::: 250 500 1000 2000 5000 ::: 10 > output.txt *)
(* there is sometimes a data race and dune throws an error; just try again until it works *)
let () =
  Random.self_init ();
  let size = int_of_string (Array.get Sys.argv 1) in
  let trials = int_of_string (Array.get Sys.argv 2) in
  let pca_dims = int_of_string (Array.get Sys.argv 3) in
  let digits = read_digits "digits.txt" points dims in
  let labels = read_labels "labels.txt" points in
  let data = pca digits pca_dims in
  let knn_params = [(1, 1); (5, 1); (10, 1); (1, 2); (5, 2); (10, 2); (1, -1); (5, -1); (10, -1)] in
  List.iter print_outcome (run_seq_test knn_params data labels size trials);