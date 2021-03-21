(* Coursera Programming Languages, Assignment 2, L. Wieckowski, 2021 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (x, xs) =
    let fun reverse (xs, res) = case xs of [] => res | hd::tl => reverse(tl, hd::res)
	fun aux (visited, xs) =
	    case xs of
		[] => NONE
	      | hd::tl => if same_string(hd, x)
			      then SOME (reverse (visited, []) @ tl)
			      else aux (hd::visited, tl)
    in aux ([], xs) end

fun get_substitutions1 (subs, s) =
    case subs of
	[] => []
      | hd::tl => case all_except_option (s, hd) of
			 NONE => get_substitutions1 (tl, s)
		       | SOME sub => sub @ get_substitutions1 (tl, s)

fun get_substitutions2 (subs, s) =
    let fun aux (subs, acc) =
	    case subs of
		[] => acc
	      | hd::tl => case all_except_option (s, hd) of
			      NONE => aux (tl, acc)
			    | SOME sub => aux (tl, acc @ sub)
    in aux (subs, []) end

fun similar_names (subs, {first, middle, last}) =
    let val sub = get_substitutions2 (subs, first)
	fun aux (sub, ans) =
	    case sub of
		[] => {first=first, middle=middle, last=last}::ans
	      | hd::tl => aux(tl, {first=hd, middle=middle, last=last}::ans)
    in aux(sub, []) end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card =
    case card of
	(Spades, _) => Black
      | (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value card =
    case card of
	(_, Num(value)) => value
      | (_, Ace) => 11
      | (_, _) => 10

fun remove_card (cs, c, e) =
    let fun aux (cs, acc) =
	    case cs of
		[] => raise e
	      | hd::tl => if hd = c
			  then acc @ tl
			  else aux (tl, hd::acc)
    in aux (cs, []) end

fun all_same_color cs =
    case cs of
	hd::nk::tl => card_color (hd) = card_color (nk) andalso all_same_color (nk::tl)
      | _ => true

fun sum_cards cs =
    let fun sum (cs, acc) =
	    case cs of
		[] => acc
	      | hd::tl => sum (tl, acc + card_value (hd))
    in sum (cs, 0) end

fun score (cs, goal) =
    let fun prelim_score (sum) =
	    if sum > goal
	    then 3 * (sum - goal)
	    else goal - sum
    in
	if all_same_color cs then prelim_score (sum_cards cs) div 2 else prelim_score (sum_cards cs)
    end

fun officiate (cards, moves, goal) =
    let fun aux(cards, held_cards, moves) =
	    case  (cards, moves) of
		(_, []) => score(held_cards, goal)
	      | (_, (Discard c)::mtl) => let val held = remove_card(held_cards, c, IllegalMove)
					in aux(cards, held, mtl) end
	      | ([], (Draw)::_) => score(held_cards, goal)
	      | (chd::ctl, (Draw)::mtl) => let val held = chd::held_cards
				       in
					   if sum_cards(held) > goal
					   then score(held, goal)
					   else aux(ctl, held, mtl)
				       end
    in aux(cards, [], moves) end
