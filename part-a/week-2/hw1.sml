(* Coursera Programming Languages Part A, Homework 1, L. Wieckowski, 2021 *)

fun is_older (date1 : int * int * int, date2 : int * int * int) =
    let val y1 = #1 date1
	val y2 = #1 date2
	val m1 = #2 date1
	val m2 = #2 date2
	val d1 = #3 date1
	val d2 = #3 date2
    in
	y1 < y2 orelse
	y1 = y2 andalso m1 < m2 orelse
	y1 = y2 andalso m1 = m2 andalso d1 < d2
    end

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates then 0
    else
	let val m = #2 (hd dates)
	    val n = number_in_month (tl dates, month)
	in
	    if month = m
	    then n + 1
	    else n
	end

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months then 0
    else number_in_month (dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates then []
    else
	let val date = hd dates
	    val matching_dates = dates_in_month (tl dates, month)
	in
	    if #2 date = month
	    then date::matching_dates
	    else matching_dates
	end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months then []
    else dates_in_months (dates, tl months) @ dates_in_month (dates, hd months)


(* Tests *)

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
val test37 = dates_in_months ([(2020,12,12), (2021,11,23)], [11, 12, 9]) = [(2020,12,12), (2021,11,23)]
