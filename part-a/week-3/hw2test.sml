use "hw2solution.sml";

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1a1 = all_except_option ("A", ["A"]) = SOME []
val test1a2 = all_except_option ("A", ["B"]) = NONE
val test1a3 = all_except_option ("C", ["A", "B", "C", "D"]) = SOME ["A", "B", "D"]

val test1b1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test1b2 = get_substitutions1 ([["foo", "bar"],["there", "car"], ["foo", "nox"]], "foo") = ["bar", "nox"]

val test1c1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test1c2 = get_substitutions2 ([["foo", "bar"],["there", "car"], ["foo", "nox"]], "foo") = ["bar", "nox"]

val test1d1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
			   {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}]

val test2a = card_color (Clubs, Num 2) = Black

val test2b1 = card_value (Clubs, Num 2) = 2
val test2b2 = card_value (Diamonds, Ace) = 11
val test2b3 = card_value (Hearts, Jack) = 10

val test2c1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test2c2 = remove_card ([(Hearts, King), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, King)]

val test2d1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test2d2 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Num(2)), (Spades, Queen)] = false

val test2e1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test2f1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test2f2 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2

val test2g1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test2g2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
			[Draw,Draw,Draw,Draw,Draw],
			42)
	     = 3
val test2g3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
			 [Draw,Discard(Hearts,Jack)],
			 42);
	       false)
	      handle IllegalMove => true)
