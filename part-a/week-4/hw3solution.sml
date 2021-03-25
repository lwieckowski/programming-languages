(* Coursera Programming Languages, Homework 3, L. Wieckowski, 2021 *)

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

fun only_capitals xs =
    List.filter (fn s => Char.isUpper (String.sub (s, 0))) xs

fun longest_string1 xs =
    List.foldl (fn (s, acc) => if String.size(s) > String.size(acc)
			       then s
			       else acc) "" xs

fun longest_string2 xs =
    List.foldl (fn (s, acc) => if String.size(s) >= String.size(acc)
			       then s
			       else acc) "" xs

fun longest_string_helper f xs =
    List.foldl (fn (s, acc) => if f (String.size(s), String.size(acc))
			       then s
			       else acc) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => if f x = SOME x
		  then x
		  else first_answer f xs'

fun all_answers f xs =
    let fun helper (ys, acc) =
	    case ys of
		[] => SOME acc
	      | y::ys' => case f y of
			      SOME y => helper (ys', acc @ y)
			    | NONE => NONE
    in helper (xs, []) end

fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size(x)) p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let fun vars pat =
	    case pat of
		Variable s => [s]
	      | TupleP pats => foldl (fn (pat', acc) => case pat' of
						       Variable s' => s'::acc
						     | _ => acc) [] pats
	      | ConstructorP (s, pat') => vars pat'
	      | _ => []
	fun all_distinct strs =
	    case strs of
		[] => true
	      | str::strs' => if List.exists (fn x => str = x) strs'
			      then false
			      else all_distinct strs'
    in
	all_distinct (vars p)
    end



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
(*
val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

*)
