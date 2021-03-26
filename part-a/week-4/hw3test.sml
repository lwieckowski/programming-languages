use "hw3solution.sml";

(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(* tests *)

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["A","B","c"] = ["A","B"]
val test1c = only_capitals ["A","basic","Cis"] = ["A","Cis"]

val test2 = longest_string1 ["A","bc","CC"] = "bc"

val test3 = longest_string2 ["A","bc","CC"] = "CC"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","CC", "BB"] = "CC"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1] = SOME [1,1,1]
val test8c = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []

val test9a1 = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (TupleP [Wildcard, ConstP 1, Wildcard]) = 2

val test9b1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "abc", Wildcard]) = 5

val test9c1 = count_some_var ("x", Variable("x")) = 1
val test9c2 = count_some_var ("c", TupleP [Variable("c"),
					   Variable("b"),
					   Wildcard,
					   Variable("c")]) = 2

val test10a = check_pat (Variable("x")) = true
val test10b = check_pat (TupleP [Variable("x"), Wildcard, Variable("x")]) = false

val test11a = match (Const(1), UnitP) = NONE
val test11b = match (Const(2), Variable "a") = SOME [("a", Const(2))]

val test12a = first_match Unit [UnitP] = SOME []
val test12b = first_match (Const(1)) [UnitP, Variable "s"] = SOME [("s", Const(1))]
