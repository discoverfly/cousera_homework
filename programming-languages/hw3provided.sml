(* Coursera Programming Languages, Homework 3, Provided Code *)


val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x, 0)))

val longest_string1 = foldl (fn (x, y) => if String.size x > String.size y then x else y) ""

val longest_string2 = foldl (fn (x, y) => if String.size x >= String.size y then x else y) ""

fun longest_string_helper f xs = foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode


exception NoAnswer

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME(v) => v
		    | NONE => first_answer f xs' 

fun all_answers f xs =
    case xs of
	[] => SOME([])
      | x::xs' => let val t = all_answers f xs'
		  in
		      case (f x, t) of
			  (SOME(v), SOME(w)) => SOME(v @ w)
			| _ => NONE
		  end

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

fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p
fun count_some_var(s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let
	fun get_variables pat =
	    case pat of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (x, y) => (get_variables x) @ y) [] ps
	      | ConstructorP(_, tp) => get_variables tp
	      | _ => []
	fun no_repeat xs =
	    case xs of
		[] => true
	      | x::xs' => if List.exists (fn y => x = y) xs' then false else no_repeat xs' 
    in
	no_repeat (get_variables p)
    end

fun match(v, p) =
    case (p, v) of
	(Wildcard, _) => SOME([])
      | (Variable s, tv) => SOME([(s, v)])  
      | (UnitP, Unit) => SOME([])
      | (ConstP x, Const y) => if x = y then SOME([]) else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs then all_answers (fn (tp, tv) => match(tv, tp)) (ListPair.zip(ps, vs)) else NONE 
      | (ConstructorP(s1, tp), Constructor(s2, tv)) => if s1 = s2 then match(tv, tp) else NONE
      | _ => NONE

fun first_match v ps = SOME(first_answer (fn p => match(v, p)) ps) handle No_answer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

