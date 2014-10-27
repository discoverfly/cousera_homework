(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s, slist) =  
    case slist of
	[] => NONE
      | x::xs => if same_string(s, x) then SOME(xs)
		 else case all_except_option(s, xs) of
			  NONE => NONE
			| SOME(res)  => SOME(x::res) 

fun get_substitutions1(xss, x) =
    case xss of
	[] => []
      | xs::xss' => case all_except_option(x, xs) of
			NONE => get_substitutions1(xss', x)
		      | SOME(ys) => ys @ get_substitutions1(xss', x) 

fun get_substitutions2(xss, x) =
    let
	fun sub_fun(xss, x, res) =
	    case xss of 
		[] => res
	      | xs::xss' => case all_except_option(x, xs) of
				NONE => sub_fun(xss', x, res)
			      | SOME(ys) => sub_fun(xss', x, res @ ys) 
    in
	sub_fun(xss, x, [])
    end

fun similar_names(xss, full_name) =
    case full_name of
	{first = f, middle = m, last = l} =>
	let
	    fun sub_fun(xs: string list) = 
		case xs of
		    [] => []
		  | x::xs' => {first = x, middle = m, last = l} :: sub_fun(xs')
	in
	    sub_fun(f::get_substitutions1(xss, f))
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

fun card_color x = 
    case x of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | _ => Red

fun card_value x = 
    case x of
	(_, Num(i)) => i
      | (_, Ace) => 11
      | _ => 10

fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => if x = c then xs else x::remove_card(xs, c, e)
fun all_same_color cs =
    case cs of 
	x::(y::zs) => (card_color x = card_color y) andalso all_same_color(y::zs)
      | _ => true
fun sum_cards cs =
    let
	fun acc(cs, sum) =
	    case cs of
		[] => sum
	      | c::cs' => acc(cs', sum + (card_value c))
    in
	acc(cs, 0)
    end

fun score(cs, goal) =
    let 
	val sum = sum_cards(cs)
	val pri_score = if sum > goal then 3*(sum - goal) else goal - sum
    in
	if all_same_color(cs) then pri_score div 2 else pri_score
    end

fun officiate(cs, ms, goal) =
    let
	fun run(cs, ms, hs) =
	    case (ms, cs) of
		([], _) => score(hs, goal)
	      | (Draw::ms', []) => score(hs, goal)
	      | (Draw::ms', c::cs') => if sum_cards(c::hs) > goal then score(c::hs, goal)
				       else run(cs', ms', c::hs)
	      | (Discard(c)::ms', _) => run(cs, ms', remove_card(hs, c, IllegalMove)) 
    in
	run(cs, ms, [])
    end
