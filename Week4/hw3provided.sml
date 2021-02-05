(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

			       (*
fun only_capitals(lst) =
    let fun f s = Char.isUpper(String.sub(s,0))
    in
	List.filter f lst
    end

fun oc2(lst) =
    List.filter(fn x => Char.isUpper(String.sub(x, 0))) lst
			       *)
			       
val only_capitals  =
    List.filter(fn x => Char.isUpper(String.sub(x, 0)))


fun longest_string1 xs =
    foldl (fn (x,y) =>
	      if String.size(x) > String.size(y)
	      then x
	      else y
	  ) "" xs


fun longest_string2 xs =
    foldl (fn (x,y) =>
	      if String.size x >= String.size y
	      then x
	      else y
	  ) "" xs


fun longest_string_helper f xs =
    foldl (fn (x,y) =>
	      if f(String.size(x), String.size(y))
	      then x
	      else y
	  ) "" xs


fun greater x y =
    x > y
	      	      
val longest_string3 =
    longest_string_helper (fn (x,y) => x > y)

val longest_string4 =
    longest_string_helper (fn (x,y) => x >= y)


val longest_capitalized = 
    longest_string1 o only_capitals


val rev_string =
    String.implode o rev o String.explode


fun first_answer f [] = raise NoAnswer
  | first_answer f (x::xs') =
    case f x of
	SOME y => y
      | NONE => first_answer f xs' 


fun all_answers f [] = SOME []
  | all_answers f xs =
    let fun loop(acc, []) = SOME []
	  | loop(acc, SOME(y)::ys) = loop(acc @ y, ys)
	  | loop(acc, NONE::ys) = NONE
    in
	loop([], map f xs)
    end
	

fun count_wildcards(p) =
    g (fn () => 1) (fn x => 0) p
    

fun count_wild_and_variable_lengths p =
    g (fn () => 1) String.size(x) p


fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x=s then 1 else 0) p



fun has_repeats(sl) =
    case sl of [] => false
	     | x::xs => List.exists (fn a => a = x) xs orelse has_repeats(xs)

fun extract_strings(p) =
    case p of
	Variable x => [x]
      | TupleP ps => List.foldl(fn (v, acc) => acc @ extract_strings(v)) [] ps
      | _ => []
		 
									 
fun check_pat(p) =
    let
	fun extract_strings(p) =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl(fn (v, acc) => acc @ extract_strings(v)) [] ps
	      | ConstructorP(_,p) => extract_strings(p)  
	      | _ => []
	fun has_repeats([]) = false
	  | has_repeats(x::xs) = List.exists (fn x' => x' = x) xs orelse has_repeats(xs)							  
    in
	(not o has_repeats o extract_strings) p
    end


fun match(v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (v, Variable s) => SOME[(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP cp) => if c = cp then SOME [] else NONE 
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1 = s2 
						  then match(v,p)
						  else NONE
      | (_, _) => NONE
				 
fun first_match v ps =
    (SOME (first_answer (fn p => match (v, p)) ps)) handle NoAnswer => NONE
