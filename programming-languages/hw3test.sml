
use "hw3provided.sml";

val t1_1 = only_capitals ["A", "abc", "Xab", "yAF"]  = ["A", "Xab"]
val t1_1 = only_capitals [] = []

val t2_1 = longest_string1 [] = ""
val t2_2 = longest_string1 ["A", "ABD", "AC"] = "ABD"
val t2_3 = longest_string1 ["ABC", "BCD", "DEF"]  = "ABC"

val t3_1 = longest_string2 [] = ""
val t3_2 = longest_string2 ["A", "ABD", "AC"] = "ABD"
val t3_3 = longest_string2 ["ABC", "BCD", "DEF"]  = "DEF"

val t4_1 = longest_string3 [] = ""
val t4_2 = longest_string3 ["A", "ABD", "AC"] = "ABD"
val t4_3 = longest_string3 ["ABC", "BCD", "DEF"]  = "ABC"

val t4_4 = longest_string4 [] = ""
val t4_5 = longest_string4 ["A", "ABD", "AC"] = "ABD"
val t4_6 = longest_string4 ["ABC", "BCD", "DEF"]  = "DEF"

val t5_1 = longest_capitalized ["a", "bc", "d"]  = ""
val t5_2 = longest_capitalized ["aaaa",  "Bcc", "adf", "Dee", "adef"]  = "Bcc"

val t6_1 = rev_string "abcdef" = "fedcba"

val t9_1 = count_wildcards (TupleP [Wildcard, Wildcard, UnitP, Wildcard]) = 3
val t9_2 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "abc", UnitP]) = 4
val t9_3 = count_some_var "abc" (TupleP [Wildcard, Variable "abc", Variable "a", Variable "ABC", Variable "abc"]) = 2

val t10_1 = check_pat (TupleP [Wildcard]) = true
val t10_2 = check_pat (TupleP [Wildcard, Variable "a", Variable "b"]) = true
val t10_3 = check_pat (TupleP [Wildcard, Variable "a", Variable "a"]) = false
