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

(* Run-length encoding *)

let rec encode = function
  | [] -> [] 
  | h :: t -> let e = encode t in begin match e with
    | [] -> [(1, h)]
    | (c, i) :: et -> if h = i
      then (c + 1, i) :: et
      else (1, h) :: e
    end
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

(* Modified run-length encoding *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let rec encode = function
  | [] -> [] 
  | h :: t -> let e = encode t in begin match e with
    | [] -> [One h]
    | One i :: et -> if h = i
      then Many (2, i) :: et
      else One h :: e
    | Many (c, i) :: et -> if h = i
      then Many (c + 1, i) :: et
      else One h :: e
    end
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

(* Decode a run-length encoded list *)

let rec decode = function
  | [] -> []
  | One i :: t -> i :: decode t
  | Many (c, i) :: t -> if c = 1
    then i :: decode t
    else i :: decode (Many (c - 1, i) :: t)
;;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;

(* Duplicate the elements of a list *)
let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t
;;

duplicate ["a"; "b"; "c"; "c"; "d"];;

(* Repeat the elements of a list a given number of times *)

let rec replicate lst n =
  if n = 1
    then lst
    else match lst with
      | [] -> []
      | h :: t -> h :: replicate [h] (n - 1) @ replicate t n
;;

replicate ["a"; "b"; "c"] 3;;

(* Drop every nth element from a list *)

let rec drop_aux lst n c =
  match lst with
    | [] -> []
    | h :: t -> if c = 1
      then drop_aux t n n
      else h :: drop_aux t n (c - 1)
;;

let drop lst n = drop_aux lst n n
;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

(* Split a list into two parts; the length of the first part is given *)

let rec split lst n =
  if n = 0
    then ([], lst)
    else match lst with
      | [] -> ([], [])
      | h :: t -> let (b, e) = split t (n - 1) in (h :: b, e)
;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

split ["a"; "b"; "c"; "d"] 5;;

(* Extract a slice from a list *)

let rec slice lst i k =
  if k < 0
    then []
    else match lst with
      | [] -> []
      | h :: t -> if i <= 0
        then h :: slice t (i - 1) (k - 1)
        else slice t (i - 1) (k - 1)
;;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
