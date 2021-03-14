(* Coursera Programming Languages Part A, Homework 1, L. Wieckowski, 2021 *)

fun is_older (date1 : int * int * int, date2 : int * int * int) =
    #1 date1 < #1 date2 orelse
    #1 date1 = #1 date2 andalso #2 date1 < #2 date2 orelse
    #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates then 0
    else
	let val n = number_in_month (tl dates, month)
	in
	    if month = #2 (hd dates)
	    then n + 1
	    else n
	end

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months then 0
    else number_in_month (dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates then []
    else
	let val matching_dates = dates_in_month (tl dates, month)
	in
	    if #2 (hd dates) = month
	    then (hd dates)::matching_dates
	    else matching_dates
	end

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months then []
    else dates_in_months (dates, tl months) @ dates_in_month (dates, hd months)

fun get_nth (words : string list, n : int) =
    if n = 1
    then hd words
    else get_nth (tl words, n-1)

fun date_to_string (date : int * int * int) =
    let val months = ["January", "February", "March", "April",
		      "May", "June", "July", "August", "September",
		      "October", "November", "December"]
	val year = Int.toString (#1 date)
	val month = get_nth (months, #2 date)
	val day = Int.toString (#3 date)
    in month ^ " " ^ day ^ ", " ^ year end

fun number_before_reaching_sum (sum : int, numbers : int list) =
    if sum <= hd numbers
    then 0
    else number_before_reaching_sum (sum - hd numbers, tl numbers) + 1

fun what_month (day_in_year_number : int) =
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum (day_in_year_number, days_in_months) + 1 end

fun month_range (day1 : int, day2: int) =
    if day1 > day2 then []
    else  what_month(day1)::month_range(day1 + 1, day2)

fun oldest  (dates : (int * int * int) list) =
    if null dates then NONE
    else
	let val tail_oldest = oldest (tl dates)
	in
	    if isSome tail_oldest andalso is_older (valOf tail_oldest, hd dates)
	    then tail_oldest
	    else SOME (hd dates)
	end

(* Challenge problem 12 solution *)

fun contains (numbers : int list, number : int) =
    if null numbers then false
    else
	if hd numbers = number then true
	else contains (tl numbers, number)

fun remove_duplicates (numbers : int list) =
    if null numbers then []
    else
	if contains (tl numbers, hd numbers)
	then remove_duplicates (tl numbers)
	else (hd numbers)::remove_duplicates (tl numbers)

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let val month_set = remove_duplicates (months)
    in number_in_months (dates, month_set) end

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    let val month_set = remove_duplicates (months)
    in dates_in_months (dates, month_set) end
