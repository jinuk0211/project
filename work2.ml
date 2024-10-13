
let product : int list -> int = fun lst ->
  List.fold_left (fun acc x -> acc * x) 1 lst

let () =
  Printf.printf "product [2; 3; 4] = %d\n" (product [2; 3; 4]);
  Printf.printf "product [6] = %d\n" (product [6]);
  Printf.printf "product [] = %d\n" (product [])

let rec count_true : ('a -> bool) -> 'a list -> int = fun p lst ->
  match lst with
  | [] -> 0
  | x :: xs -> (if p x then 1 else 0) + count_true p xs

let () =
  let result = count_true (fun x -> x > 0) [1; 2; -3; 4; 0] in
  let result2 = count_true (fun x -> x = "x") ["a"; "b"; "c"] in
  Printf.printf "1. %d\n" result;
  Printf.printf "2. %d\n" result2



let rec iter : int * ('a -> 'a) -> 'a -> 'a = fun (n, f) ->
  if n = 0 then
    (fun x -> x) 
  else
    (fun x -> f ((iter (n-1, f)) x))

let () = 
  let result = (iter (1, fun x -> x * x)) 4 in
  Printf.printf "iter(1, fun x -> x * x) 4 = %d\n" result;
  
  let result2 = (iter (4, fun x -> x * x)) 10 in
  Printf.printf "iter(4, fun x -> x * x) 10 = %d\n" result2;
  
  let result3 = (iter (0, fun x -> x * 5)) 10 in
  Printf.printf "iter(0, fun x -> x * 5) 10 = %d\n" result3
