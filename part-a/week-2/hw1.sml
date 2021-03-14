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

fun get_nth (words : string list, n : int) =
    if null words then hd words
    else
	if n = 1
	then hd words
	else get_nth (tl words, n-1)

fun date_to_string (date : int * int * int) =
    let val months = ["January", "February", "March", "April", "May", "June",
		      "July", "August", "September", "October", "November", "December"]
	val year = Int.toString (#1 date)
	val month = get_nth (months, #2 date)
	val day = Int.toString (#3 date)
    in month ^ " " ^ day ^ ", " ^ year end

fun number_before_reaching_sum (sum : int, numbers : int list) =
    if null numbers then 0
    else
	let val remaining_sum = sum - hd numbers
	    val counter = number_before_reaching_sum (remaining_sum, tl numbers)
	in
	    if remaining_sum > 0
	    then counter + 1
	    else counter
	end

fun what_month (day_in_year_number : int) =
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum (day_in_year_number, days_in_months) + 1
    end

fun month_range (day1 : int, day2: int) =
    if day1 > day2 then []
    else
	let val month = what_month(day1)
	in if day1 = day2 then [month]
	   else month::month_range(day1 + 1, day2)
	end

fun oldest  (dates : (int * int * int) list) =
    if null dates then NONE
    else
	let val tail_oldest = oldest (tl dates)
	in
	    if isSome tail_oldest andalso is_older (valOf tail_oldest, hd dates)
	    then tail_oldest
	    else SOME (hd dates)
	end
