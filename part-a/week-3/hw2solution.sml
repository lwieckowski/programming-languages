(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (word, words) =
    let fun aux (visited, xs) =
	    case xs of
		[] => NONE
	      | head::tail => if same_string(head, word)
			      then SOME (visited @ tail)
			      else aux (head::visited, tail)
    in aux ([], words) end


fun get_substitutions1 (substitutions, s) =
    case substitutions of
	[] => []
      | hd::tl => let val sub_opt = all_except_option (s, hd)
		  in case sub_opt of
			 NONE => get_substitutions1 (tl, s)
		       | SOME sub => sub @ get_substitutions1 (tl, s)
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
