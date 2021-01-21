fun is_older_old (x : int*int*int, y : int*int*int) =
    #3 x < #3 y orelse
    #1 x < #1 y orelse
    #2 x < #2 y

fun is_older (x : int*int*int, y : int*int*int) =
    if #1 x < #1 y
    then true
    else if #1 x > #1 y
    then false
    else
	if #2 x < #2 y
	then true
	else if #2 x > #2 y
	then false
	else
	    if #3 x < #3 y
	    then true
	    else false
		 


fun number_in_month (dates : (int * int * int) list, month : int) =
    let fun in_month (date : int * int * int, month : int) =
	    #2 date = month
    in
	if null dates
	then 0
	else
	    if in_month(hd dates, month)
	    then 1 + number_in_month(tl dates, month)
	    else number_in_month(tl dates, month)
    end
			  

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else
	number_in_month(dates, hd months) +
	number_in_months(dates, tl months)
		       
	      
fun dates_in_month(dates : (int * int * int) list, month : int) =
    let fun in_month (date : int * int * int, month : int) =
	    #2 date = month
    in
	if null dates
	then []
	else
	    if in_month(hd dates, month)
	    then (hd dates) :: dates_in_month(tl dates, month)
	    else dates_in_month(tl dates, month)
    end
	
fun dates_in_months(dates : (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @
	 dates_in_months(dates, tl months)


fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)

fun date_to_string(date : (int * int * int)) =
    let
	val dayStr  = Int.toString(#3 date)
	val month   = #2 date
	val yearStr = Int.toString(#1 date)
	val months = ["January", "February", "March", "April", "May", "June", "July",
		      "August", "September", "October", "November", "December"]
		  
    in
	get_nth(months, month)^"-"^dayStr^"-"^yearStr
    end


fun number_before_reaching_sum(sum : int, xs : int list) =
    let
	fun sum_upto(n : int, cumsum : int, ys : int list) =
	    if cumsum < sum andalso
	       cumsum + (hd ys) >= sum
	    then n
	    else
		sum_upto(n+1, cumsum + (hd ys), (tl ys))
    in
	sum_upto(0, 0, xs)
    end


fun what_month(day : int) =
    let
	val daysInMonth = [31, 28, 31, 30, 31, 30,
			   31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, daysInMonth) + 1
    end
	
			 
fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else if day1 = day2
    then what_month(day2):: []
    else what_month(day1) :: month_range(day1+1,day2)


fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let val tl_ans = oldest(tl dates)
	in if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
	   then tl_ans
	   else SOME (hd dates)
	end

fun remove_dups(xs : int list) =
    let 
	fun remove_from_list(y : int, ys : int list) =
    	    if null ys
	    then []
	    else if y = (hd ys)
	    then remove_from_list(y, tl ys)
	    else (hd ys) :: remove_from_list(y, tl ys)
    in
	if null xs
	then []
	else
	    (hd xs) :: remove_dups(remove_from_list(hd xs, tl xs))
    end

fun remove_from_list(y : int, ys : int list) =
    if null ys
    then []
    else if y = (hd ys)
    then remove_from_list(y, tl ys)
    else (hd ys) :: remove_from_list(y, tl ys)


fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_dups(months))
		    
		  
fun dates_in_months_challenge (dates : (int * int * int) list, months: int list) =
    dates_in_months(dates, remove_dups(months))
