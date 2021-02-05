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

fun in_list(s, []) = false
  | in_list (s, x :: xs) = 
    same_string(s, x) orelse in_list(s, xs)

fun remove_from_list (s, []) = []
  | remove_from_list (s, x :: xs) = 
    if same_string(s, x)
    then remove_from_list(s, xs)
    else x::remove_from_list(s, xs)

			     
fun all_except_option(s : string, lst : string list) =
    if in_list(s, lst)
    then SOME (remove_from_list(s, lst))
    else NONE

							     
fun get_substitutions1( [], _) = []
  | get_substitutions1(xs::subs', s) = 
    case all_except_option(s, xs) of
	NONE => get_substitutions1(subs', s)
      | SOME ys => ys @ get_substitutions1( subs', s)

							     
fun get_substitutions2( subs, s) =
    let fun aux([], acc) = acc
	  | aux(xs::subs', acc) = 
	    case all_except_option(s, xs) of
		NONE => aux(subs', acc)
	      | SOME ys => aux(subs', acc @ ys)
    in
	aux(subs, [])
    end


fun similar_names(subs, {first=f, middle=m, last=l}) =
    let
	fun sub_name (sub) = {first=sub, middle=m, last=l}
	fun substitute ([]) = []
	  | substitute (x::xs) = sub_name(x) :: substitute(xs)
    in
	{first=f, middle=m, last=l} :: substitute(get_substitutions2(subs, f))
    end

fun similar_names_acc(subs, {first=f, middle=m, last=l}) =
    let
	fun sub_name (sub) = {first=sub, middle=m, last=l}
	fun substitute ([], acc) = acc
	  | substitute (x::xs, acc) = substitute(xs, sub_name(x)::acc)
    in
	{first=f, middle=m, last=l} :: substitute(get_substitutions2(subs, f),[])
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
fun card_color(Clubs, _) = Black
  | card_color(Spades,_) = Black
  | card_color(Hearts,_) = Red
  | card_color(Diamonds,_) = Red 

(*
fun card_value(s, n) =
    case n of
	Num n => n
*)
		     
fun card_value(_, Num n) = n
  | card_value(_, Ace) = 11
  | card_value(_, _) = 10

			   
fun remove_card([], c, e) = raise e
  | remove_card(x::cs', c, e) =
    if x=c
    then cs'
    else x::remove_card(cs', c, e)
    

fun all_same_color_old([]) = true
  | all_same_color_old(x::xs') =
    case xs' of
	[] => true
      | y::ys' => card_color(x) = card_color(y) andalso
		 all_same_color_old(xs')
				   
fun all_same_color([]) = true
  | all_same_color(x::[]) = true 
  | all_same_color(head::(neck::rest)) =
    card_color(head) = card_color(neck) andalso all_same_color((neck::rest))

							      
fun sum_cards_old(cs) =
    let
	fun f(cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => f(cs',acc+card_value(c))
    in
	f(cs,0)
    end

fun sum_cards(cs) =
    let
	fun f([], acc) = acc
	  | f(c::cs', acc) = f(cs',acc+card_value(c))
    in
	f(cs,0)
    end


fun score(cards, goal) =
    let
	val sum = sum_cards(cards)
	val prelim_score =
	    if sum > goal
	    then 3 * (sum - goal)
	    else (goal - sum)
    in
	if all_same_color(cards)
	then (prelim_score div 2)
	else prelim_score
    end


(*
fun officiate(cards, moves, goal) =
    let
	fun continue_play(held_cards, card_list, move_list) =
	    case move_list of
		[] => score(held_cards, goal)
	      | (Draw)::rest =>
		(case card_list of
		    [] => score(held_cards, goal)
		  | c::cs' =>
		    if sum_cards(c::held_cards) > goal
		    then score(c::held_cards, goal)
		    else continue_play(c::held_cards, cs', rest)
		)
	      | (Discard c)::rest =>
		continue_play(held_cards, remove_card(card_list, c, IllegalMove), rest)
    in
	continue_play([], cards, moves)
    end
*)



fun officiate(card_list, move_list, goal) =
    let
	fun continue_play(cards, moves, held) =
	    case (cards, moves) of
		(_, []) => score(held, goal)
	      | ([], Draw::moves') => score(held, goal)
	      | (card::cards', move::moves') =>
		case move of
		    Discard c => continue_play(held, moves', remove_card(cards, c, IllegalMove))
		  | Draw => if sum_cards(card::held) > goal
			    then score(card::held, goal)
			    else continue_play(cards', moves', card::held) 

    in
	continue_play(card_list, move_list, [])
    end
