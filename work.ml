
let rec fibonacci : int -> int = fun n ->
  if n < 0 then
    invalid_arg "fibonacci: n must be non-negative"
  else if n = 0 then
    0
  else if n = 1 then
    1
  else
    fibonacci (n - 1) + fibonacci (n - 2)


let () =
  Printf.printf "fibonacci 0 = %d\n" (fibonacci 0);
  Printf.printf "fibonacci 1 = %d\n" (fibonacci 1);
  Printf.printf "fibonacci 8 = %d\n" (fibonacci 8);
  Printf.printf "fibonacci 11 = %d\n" (fibonacci 11)



(* 2. Max function (recursive version) *)
let rec max : int list -> int = function
  | [] -> min_int
  | h :: t -> let max_tail = max t in
              if h > max_tail then h else max_tail

(* 3. Max function (using fold_left) *)
let max_fold : int list -> int = fun lst ->
  match lst with
  | [] -> min_int
  | h :: t -> List.fold_left (fun acc x -> if x > acc then x else acc) h t

(* Test cases for max functions *)
let test_max lst =
  Printf.printf "Input: [%s]\n" (String.concat "; " (List.map string_of_int lst));
  Printf.printf "max (recursive) = %d\n" (max lst);
  Printf.printf "max_fold = %d\n\n" (max_fold lst)


(* Swapper function *)
let rec swapper : 'a -> 'a -> 'a list -> 'a list = fun x y l ->
  match l with
  | [] -> []
  | h :: t -> 
      if h = x then
        y :: swapper x y t
      else
        h :: swapper x y t

(* Test cases for swapper function *)
let test_swapper x y l =
  Printf.printf "Input: x = %s, y = %s, l = [%s]\n" 
    (Printf.sprintf "%S" x) 
    (Printf.sprintf "%S" y) 
    (String.concat "; " (List.map (Printf.sprintf "%S") l));
  let result = swapper x y l in
  Printf.printf "Output: [%s]\n\n" 
    (String.concat "; " (List.map (Printf.sprintf "%S") result))

let () =
  test_swapper "apple" "orange" ["apple"; "banana"; "apple"; "cherry"];
  test_swapper "x" "y" ["x"; "x"; "x"]


(* Previous functions... *)

(* Zipper function *)
let rec zipper : 'a list -> 'a list -> 'a list = fun a b ->
  match (a, b) with
  | ([], []) -> []
  | ([], bs) -> bs
  | (as', []) -> as'
  | (x :: xs, y :: ys) -> x :: y :: zipper xs ys

(* Test cases for zipper function *)
let test_zipper a b =
  Printf.printf "Input: a = [%s], b = [%s]\n" 
    (String.concat "; " (List.map string_of_int a))
    (String.concat "; " (List.map string_of_int b));
  let result = zipper a b in
  Printf.printf "Output: [%s]\n\n" 
    (String.concat "; " (List.map string_of_int result))

let () =
  test_zipper [1; 2; 4; 5] [6; 7; 9; 10; 11];
  test_zipper [6; 7; 9; 10; 11] [1; 2; 4; 5];
  test_zipper [] []
