(* https://ocaml.org/problems *)

(* Tail of a list *)
let rec last = function
  | [] -> None
  | h :: [] -> Some h
  | _ :: t -> last t
;;

last ["a"; "b"; "c"; "d"];;

last [];;

(* Last two elements of a list *)

let rec last_two = function
  | [] -> None
  | h :: n :: [] -> Some (h, n)
  | _ :: t -> last_two t
;;

last_two ["a"; "b"; "c"; "d"];;

last_two ["a"];;

(* Nth element of a list *)

let rec nth lst n =
  match lst with
  | [] -> None
  | h :: t -> begin
    match n with
    | 0 -> Some h
    | n -> nth t (n - 1)
    end
;;

nth ["a"; "b"; "c"; "d"; "e"] 2;;

nth ["a"] 2;;

(* Length of a list *)

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t
;;

length ["a"; "b"; "c"];;

length [];;

(* Reverse a list *)

let rec rev_aux acc = function
  | [] -> acc
  | h :: t -> rev_aux (h :: acc) t

let rec rev = rev_aux [] 
;;

rev ["a"; "b"; "c"];;

(* Palindrome *)

let is_palindrome lst = (lst = rev lst)
;;

is_palindrome ["x"; "a"; "m"; "a"; "x"];;

not (is_palindrome ["a"; "b"]);;

(* Flatten a list *)

type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One h :: t -> h :: flatten t
  | Many h :: t -> flatten h @ flatten t
;;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

(* Eliminate duplicates *)

let rec compress lst =
  match lst with
  | [] -> []
  | h :: t -> let c = compress t in begin
    match c with
    | [] -> lst
    | ch :: ct -> if h = ch
      then c
      else h :: c
    end
;;

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

(* Pack consecutive duplicates *)

let rec pack lst =
  match lst with
  | [] -> []
  | h :: t -> let p = pack t in begin match p with
    | [] -> [[h]]
    | [] :: pt -> failwith "should not accumulate an empty list item"
    | (phh :: pht) :: pt -> if h = phh
        then (h :: phh :: pht) :: pt
        else [h] :: p
    end
;;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
