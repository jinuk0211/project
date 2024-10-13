(* Previous functions... *)

(* 5. Product function using fold_left *)
let product : int list -> int = fun lst ->
  List.fold_left (fun acc x -> acc * x) 1 lst

(* Test cases for product function *)
let test_product lst =
  Printf.printf "Input: [%s]\n" 
    (String.concat "; " (List.map string_of_int lst));
  let result = product lst in
  Printf.printf "Output: %d\n\n" result

let () =
  test_product [2; 3; 4];
  test_product [6];
  test_product []


(* 6. Count_true function - Recursive version *)
let rec count_true : ('a -> bool) -> 'a list -> int = fun p lst ->
  match lst with
  | [] -> 0
  | x :: xs -> (if p x then 1 else 0) + count_true p xs

(* Count_true function - Using fold_left *)
let count_true_fold : ('a -> bool) -> 'a list -> int = fun p lst ->
  List.fold_left (fun acc x -> acc + if p x then 1 else 0) 0 lst

(* Test cases for count_true functions *)
let test_count_true name func p lst =
  Printf.printf "%s:\n" name;
  Printf.printf "Output: %d\n\n" (func p lst)

let () =
  test_count_true "Recursive count_true (x -> x > 0)" count_true (fun x -> x > 0) [1; 2; -3; 4; 0];
  test_count_true "Fold count_true (x -> x > 0)" count_true_fold (fun x -> x > 0) [1; 2; -3; 4; 0];


let rec iter : int * ('a -> 'a) -> 'a -> 'a = fun (n, f) ->
  if n = 0 then
    (fun x -> x)  (* Identity function *)
  else
    (fun x -> f ((iter (n-1, f)) x))

(* Test cases for iter function *)
let test_iter n f x =
  let result = (iter (n, f)) x in
  Printf.printf "iter (%d, f) %d = %d\n" n x result

let () =
  test_iter 4 (fun x -> x * x) 10;
  test_iter 1 (fun x -> x * x) 4;
  test_iter 0 (fun x -> x * 5) 10;
  
  (* Additional test case *)
  test_iter 3 (fun x -> x * 2) 1
