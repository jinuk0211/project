exception Fail 
let rec fold_right f l a =
  match l with 
  | [] -> a
  | hd::tl -> f hd (fold_right f tl a)

let rec fold_left f a l = 
  match l with 
  | [] -> a 
  | hd::tl -> fold_left f (f hd a) tl 

let rec filter f l = 
  match l with 
  | [] -> []
  | hd::tl -> if f hd then hd::(filter f tl) else filter f tl 

let rec map f l = 
  match l with 
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

  
(* 1. *)
let rec fibonacci n a = 
  match n with 
  | 0 -> a 
  | 1 -> 1  
  | _ ->  fibonacci (n-1) (fibonacci (n-2) a)

(* 2. *)
let rec max l a = 
  match l with  
  | [] -> a
  | hd::tl -> if hd > a then max tl hd else max tl a 

let f_max l = fold_left (fun x y -> if x > y then x else y) min_int l 
let ff_max l = fold_right (fun x y -> if x > y then x else y) l min_int 

(* 3. *)
let rec swapper x y l = 
  match l with 
  | [] -> []
  | hd::tl -> 
    (
      if hd = x then y::(swapper x y tl)
      else if hd = y then x::(swapper x y tl)
      else hd::(swapper x y tl)
    )

(* 4. *)
let rec zipper a b = 
  match (a, b) with 
  | (a, []) -> a 
  | ([], b) -> b 
  | (ahd::atl, bhd::btl) -> ahd::bhd::(zipper atl btl)

(* 5. *)
let rec product l = 
  match l with 
  | [] -> 1
  | hd::tl -> hd * (product tl)

let rec fold_left_product l = fold_left (fun x y -> x * y) 1 l 
let rec fold_right_product l = fold_right (fun x y -> x * y) l 1  

(* 6. *)
let rec count_true p l = 
  match l with 
  | [] -> 0 
  | hd::tl -> if p hd then 1 + (count_true p tl) else count_true p tl  

let fold_left_count_true p l = 
  fold_left (fun x y -> if p x then 1 + y else y) 0 l 

(* 7. *)
let rec iter (n, f) = 
  if n = 0 
  then 
    fun x -> x 
  else
    fun x -> f (iter (n-1, f) x)

(* 8. *)
let rec combination l = 
  match l with 
  | [] -> [[]]
  | hd::tl -> 
      let res_combinations = combination tl in 
        res_combinations @ (map (fun x -> [hd]@x) res_combinations)
