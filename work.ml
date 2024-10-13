
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



let rec max_recursive : int list -> int = function
  | [] -> min_int
  | x :: xs -> max x (max_recursive xs)

let max_fold : int list -> int = fun lst ->
  match lst with
  | [] -> min_int
  | x :: xs -> List.fold_left max x xs


let rec swapper : 'a -> 'a -> 'a list -> 'a list = fun x y l ->
  match l with
  | [] -> []
  | h :: t -> 
      if h = x then
        y :: swapper x y t
      else
        h :: swapper x y t

let () =
  let result1 = swapper "apple" "orange" ["apple"; "banana"; "apple"; "cherry"] in
  let result2 = swapper "x" "y" ["x"; "x"; "x"] in



let rec zipper : 'a list -> 'a list -> 'a list = fun a b ->
  match (a, b) with
  | ([], []) -> []
  | ([], bs) -> bs
  | (as', []) -> as'
  | (x :: xs, y :: ys) -> x :: y :: zipper xs ys

let () =
  let result1 = zipper [1; 2; 4; 5] [6; 7; 9; 10; 11] in
  let result2 = zipper [6; 7; 9; 10; 11] [1; 2; 4; 5] in
  let result3 = zipper [] [] in
