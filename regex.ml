include List

type letter = string
type alphabet = letter list
type state = int
type transition = state * letter * state
type trans_func = transition list

type regex =
  | Empty
  | Epsilon
  | Letter of letter
  | Plus of regex * regex
  | Concat of regex * regex
  | Star of regex

(* <E, Q, q0, d, F> *)
type automaton = Automaton of alphabet * (state list) * state * trans_func * (state list)

let rec big_plus letters = 
  match letters with
  | [] -> Empty
  | "" :: [] -> Epsilon
  | l :: [] -> Letter l
  | "" :: tl -> Plus (Epsilon,  big_plus tl)
  | l :: tl -> Plus ((Letter l), (big_plus tl))

let basic_regexes states delta =
  let get_letters_aux s1 s2 = 
    List.map 
      (fun (w,l,v) -> l) 
      (List.filter (fun (w,l,v) -> w = s1 && v = s2) delta)
  in let get_letters s1 s2 =
    let res = (get_letters_aux s1 s2) in
    if s1 = s2 then "" :: res else res
  in let gen_basic state =  
    List.map big_plus (List.map (get_letters state) states)
  in List.map gen_basic states

let rec reduce_once regex = 
  match regex with
  | Empty -> Empty
  | Epsilon -> Epsilon
  | Letter l -> Letter l
  | Concat (Empty, _) -> Empty
  | Concat (_, Empty) -> Empty
  | Concat (Epsilon, p) -> reduce_once p
  | Concat (p, Epsilon) -> reduce_once p
  | Concat (Star (Letter l1), Plus (Epsilon, Letter l2)) -> 
      if l1 = l2 then Star (Letter l1) else 
        Concat (Star (Letter l1), Plus (Epsilon, Letter l2)) 
  | Concat (p1, p2) -> Concat (reduce_once p1, reduce_once p2)
  | Plus (Empty, p) -> reduce_once p
  | Plus (p, Empty) -> reduce_once p
  | Plus (Epsilon, Star p) -> Star (reduce_once p)
  | Plus (Star p, Epsilon) -> Star (reduce_once p)
  | Plus (p1, p2) -> Plus (reduce_once p1, reduce_once p2)
  | Star Empty -> Empty
  | Star Epsilon -> Epsilon
  | Star (Plus (Epsilon, p)) -> Star (reduce_once p)
  | Star (Plus (p, Epsilon)) -> Star (reduce_once p)
  | Star p -> Star (reduce_once p)

let rec reduce regex = 
  let reduced = reduce_once regex in
  if reduced = regex then regex else reduce reduced

let s = [0;1;2;3;4];;
let delta = [ (0, "a", 0); (0, "b", 1); 
              (1, "a", 2); (1, "b", 1); 
              (2, "a", 0); (2, "b", 3);
              (3, "a", 4); (3, "b", 1);
              (4, "a", 4); (4, "b", 4)]

let rec iter_list = function
  | 0 -> [0]
  | n -> n :: iter_list (n - 1)

let rec iter n = List.rev (iter_list (n-1))

let increment_phis states_num regexes k = 
  let ijth i j = (List.nth (List.nth regexes i) j) in 
  let increment_regex i j =
    let rij = (ijth i j) in
    let rik = (ijth i k) in
    let rkk = (ijth k k) in
    let rkj = (ijth k j) in
    Plus (rij, Concat (rik, Concat (Star rkk, rkj))) in
  let incr_for_i i = List.map (fun j -> increment_regex i j) (iter states_num) in
  List.map incr_for_i (iter states_num)

let get_results states_num delta = 
  let states = iter states_num in
  let regexes = basic_regexes states delta in
  let rec update_regexes regexes k =
    if k = states_num then regexes else
      update_regexes (increment_phis states_num regexes k) (k+1) in
  update_regexes regexes 1

let rec string_of_regex regex = 
  match regex with
    | Empty -> "∅"
    | Epsilon -> "ε"
    | Letter l -> l
    | Plus (Letter l, r) -> l ^ "+" ^ (string_of_regex r)
    | Plus (r, Letter l) -> (string_of_regex r) ^ "+" ^ l
    | Plus (Epsilon, r) -> (string_of_regex Epsilon) ^ "+" ^ (string_of_regex r)
    | Plus (r, Epsilon) -> (string_of_regex r) ^ "+" ^ (string_of_regex Epsilon)
    | Plus (Empty, r) -> (string_of_regex Empty) ^ "+" ^ (string_of_regex r)
    | Plus (r, Empty) -> (string_of_regex r) ^ "+" ^ (string_of_regex Empty)
    | Plus (phi1, phi2) -> (string_of_regex phi1) ^ "+" ^ (string_of_regex phi2)
    | Concat (Letter l1, Letter l2) -> l1 ^ l2
    | Concat (Letter l, Epsilon) -> l ^ (string_of_regex Epsilon) 
    | Concat (Letter l, Empty) -> l ^ (string_of_regex Empty)
    | Concat (Epsilon, Letter l) -> (string_of_regex Epsilon) ^ l 
    | Concat (Empty, Letter l) -> (string_of_regex Empty) ^ l
    | Concat (Empty, Epsilon) -> (string_of_regex Empty) ^ (string_of_regex Epsilon)
    | Concat (Epsilon, Empty) -> (string_of_regex Epsilon) ^ (string_of_regex Empty)
    | Concat (Empty, Empty) -> (string_of_regex Empty) ^ (string_of_regex Empty)
    | Concat (Epsilon, Epsilon) -> (string_of_regex Epsilon) ^ (string_of_regex Epsilon)
    | Concat (phi1, phi2) ->  "(" ^ (string_of_regex phi1) ^ ")(" ^ (string_of_regex phi2) ^ ")"
    | Star (Empty) -> (string_of_regex Empty) ^ "*"
    | Star (Epsilon) -> (string_of_regex Epsilon) ^ "*"
    | Star (Letter l) -> l ^ "*"
    | Star phi -> "(" ^ (string_of_regex phi) ^ ")*"
    

let pp_print_regex fmtr f =
  Format.pp_print_string fmtr (string_of_regex f)

let basic = basic_regexes (iter 5) delta
let r1 = increment_phis 5 basic 0
let r2 = increment_phis 5 r1 1
let r3 = increment_phis 5 r2 2
let r4 = increment_phis 5 r3 3
let r5 = increment_phis 5 r4 4

let ijth i j r = (List.nth (List.nth r i) j) 
let a0 = ijth 0 0 r5
let a1 = ijth 0 1 r5
let a2 = ijth 0 2 r5
let a3 = ijth 0 3 r5
let res = Plus (Plus (Plus (reduce a0, reduce a1), reduce a2), reduce a3)
