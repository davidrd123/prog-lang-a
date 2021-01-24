(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*
fun all_except_option_old(s : string, lst : string list) =
*)

fun in_list(s, lst) =
    case lst of
	[] => false
      | hd_lst::tl_lst =>
	if same_string(s, hd_lst)
	then true
	else in_list(s, tl_lst)

fun remove_from_list(s, lst) =
    case lst of
	[] => []
      | hd_lst::tl_lst =>
	if same_string(s, hd_lst)
	then remove_from_list(s, tl_lst)
	else hd_lst::remove_from_list(s, tl_lst)

			     
fun all_except_option(s : string, lst : string list) =
    if in_list(s, lst)
    then SOME (remove_from_list(s, lst))
    else NONE
	     

fun get_substitutions1( subs, s) =
    case subs of
	[] => []
      | xs :: subs' => case all_except_option(s, xs) of
			   NONE => get_substitutions1(subs', s)
			 | SOME ys => ys @ get_substitutions1( subs', s)

							     
fun get_substitutions2( subs, s) =
    let fun aux(subs, acc) =
	    case subs of
		[] => acc
	      | xs :: subs' => case all_except_option(s, xs) of
				   NONE => aux(subs', acc)
				 | SOME ys => aux(subs', acc @ ys)
    in
	aux(subs, [])
    end


fun similar_names(subs, {first=x, middle=y, last=y}) =
    let fun helper
    in
	x ^ " " ^ y ^ " " ^ z
    end
			    
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
