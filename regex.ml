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
    | Plus (phi1, phi2) -> "(" ^ (string_of_regex phi1) ^ ") + (" ^ (string_of_regex phi2) ^ ")"
    | Concat (phi1, phi2) ->  (string_of_regex phi1) ^ (string_of_regex phi2)
    | Star phi -> "(" ^ (string_of_regex phi) ^ ")*"

let pp_print_regex fmtr f =
  Format.pp_print_string fmtr (string_of_regex f)