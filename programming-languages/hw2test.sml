
use "hw2provided.sml";

val test1 = all_except_option("a", ["b", "c"]) = NONE
val test2 = all_except_option("a", []) = NONE
val test3 = all_except_option("a", ["b", "c", "a"]) = SOME(["b", "c"])
val test4 = all_except_option("a", ["b", "a", "c"]) = SOME(["b", "c"])

val test5 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test6 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test7 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test8 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test9 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"},	{first="F", last="Smith", middle="W"}]

val test10 = remove_card([(Clubs, Num(9)), (Clubs, King), (Clubs, Ace)], (Clubs, King), IllegalMove) = [(Clubs, Num 9), (Clubs, Ace)]
val test11 = all_same_color([]) = true
val test12 = all_same_color([(Clubs, Ace)]) = true
val test13 = all_same_color([(Clubs, Ace), (Clubs, Num 9)]) = true
val test14 = all_same_color([(Clubs, Ace), (Hearts, Ace)]) = false
val test15 = all_same_color([(Clubs, Ace), (Clubs, Num 9), (Hearts, Ace)]) = false
val test16 = sum_cards([(Clubs, Ace), (Clubs, King), (Clubs, Num 3)]) = 11 + 10 + 3

val test17 = score([(Clubs, Ace), (Clubs, King), (Clubs, Num 4)], 24) = 1
val test18 = score([(Clubs, Ace), (Clubs, King), (Hearts, Num 4)], 24) = 3
val test19 = score([(Clubs, Ace)], 20) = 4
