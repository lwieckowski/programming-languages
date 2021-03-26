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
			      SOME z => helper (ys', acc @ z)
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

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP c') => if c = c' then SOME [] else NONE
      | (Constructor(s2, v'), ConstructorP(s1, p')) => if s1 = s2 then match (v', p') else NONE
      | (Tuple vs, TupleP ps) => let val pairs = ListPair.zipEq (vs, ps)
				 in
				     all_answers match pairs
				     handle UnequalLengths => NONE
				 end
      | (v, Variable s) => SOME [(s, v)]
      | (_, _) => NONE

fun first_match v ps =
    case ps of
	[] => NONE
      | p::ps' => if match (v, p) <> NONE
		  then match (v, p)
		  else first_match v ps'
