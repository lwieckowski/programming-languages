use "hw1.sml";

(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test13 = is_older ((2020,12,28),(2021,12,28)) = true
val test14 = is_older ((2020,11,28),(2020,12,28)) = true
val test15 = is_older ((2020,12,27),(2020,12,28)) = true
val test16 = is_older ((2020,12,28),(2020,12,28)) = false
val test17 = is_older ((2020,12,29),(2020,12,28)) = false
val test18 = is_older ((2020,12,28),(2020,11,28)) = false
val test19 = is_older ((2021,12,28),(2020,12,28)) = false

val test20 = number_in_month ([(2020,11,12), (2021,12,23)], 11) = 1
val test21 = number_in_month ([(2020,12,23), (2021,12,21)], 12) = 2
val test22 = number_in_month ([(2020,12,23), (2021,12,21)], 10) = 0
val test23 = number_in_month ([], 10) = 0

val test24 = number_in_months ([(2020,11,12), (2021,12,23)], [11, 10]) = 1
val test25 = number_in_months ([(2020,12,23), (2021,10,21)], [12, 10, 5]) = 2
val test26 = number_in_months ([(2020,12,23), (2021,12,21)], [10]) = 0
val test27 = number_in_months ([], [10, 6]) = 0
val test28 = number_in_months ([(2020,12,23), (2021,12,21)], []) = 0
val test29 = number_in_months ([], []) = 0

val test30 = dates_in_month ([(2020,11,12), (2021,12,23)], 11) = [(2020,11,12)]
val test31 = dates_in_month ([(2020,11,12), (2021,12,23)], 10) = []
val test32 = dates_in_month ([], 11) = []
val test33 = dates_in_month ([(2020,11,12), (2021,11,23)], 11) = [(2020,11,12), (2021,11,23)]

val test34 = dates_in_months ([(2020,11,12), (2021,12,23)], [11]) = [(2020,11,12)]
val test35 = dates_in_months ([(2020,11,12), (2021,12,23)], [10]) = []
val test36 = dates_in_months ([], [11]) = []
val test37 = dates_in_months ([(2020, 8, 12), (2020,12,12), (2021,11,23)], [11, 12, 9]) = [(2020,12,12), (2021,11,23)]

val test38 = get_nth (["9", "g", "r"], 2) = "g"

val test39 = date_to_string ((2021, 1, 14)) = "January 14, 2021"
val test40 = date_to_string ((2021, 2, 14)) = "February 14, 2021"
val test41 = date_to_string ((2021, 3, 14)) = "March 14, 2021"
val test42 = date_to_string ((2021, 4, 14)) = "April 14, 2021"
val test43 = date_to_string ((2021, 5, 14)) = "May 14, 2021"
val test44 = date_to_string ((2021, 6, 14)) = "June 14, 2021"
val test45 = date_to_string ((2021, 7, 14)) = "July 14, 2021"
val test46 = date_to_string ((2021, 8, 14)) = "August 14, 2021"
val test47 = date_to_string ((2021, 9, 14)) = "September 14, 2021"
val test48 = date_to_string ((2021, 10, 14)) = "October 14, 2021"
val test49 = date_to_string ((2021, 11, 14)) = "November 14, 2021"
val test50 = date_to_string ((2020, 12, 15)) = "December 15, 2020"

val test51 = number_before_reaching_sum (5, [1, 2, 3, 4, 5]) = 2

val test52 = what_month (1) = 1
val test53 = what_month (31) = 1
val test54 = what_month (32) = 2
val test55 = what_month (59) = 2
val test56 = what_month (60) = 3
val test57 = what_month (90) = 3
val test58 = what_month (365) = 12

val test59 = month_range (2, 1) = []
val test60 = month_range (1, 2) = [1, 1]
val test61 = month_range (30, 34) = [1, 1, 2, 2, 2]

val test62 = oldest ([(2020, 12, 28), (2021, 12, 28)]) = SOME((2020, 12, 28))
val test63 = oldest ([]) = NONE
