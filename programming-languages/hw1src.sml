
fun is_older(x: int*int*int, y: int*int*int) = 
    if #1 x <> #1 y then #1 x < #1 y
    else if #2 x <> #2 y then #2 x < #2 y
    else #3 x < #3 y

fun number_in_month(dates: (int*int*int) list, month: int) =
   if null dates then 0
   else 
       let
	   val add = if (#2 (hd dates)) = month then 1 else 0
       in
	   add + number_in_month(tl dates, month)
       end

fun number_in_months(dates: (int*int*int) list, months: int list) = 
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates then []
    else
	let
	    val tmp = dates_in_month(tl dates, month)
	in
	    if (#2 (hd dates)) = month then (hd dates) :: tmp else tmp
	end

fun dates_in_months(dates: (int*int*int) list, months) = 
    if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(slist: string list, n: int) = 
    if n = 1 then hd slist
    else get_nth(tl slist, n-1)

fun date_to_string(date: (int*int*int)) = 
    let 
	val months_names = ["January", "February", "March", "April",
		       "May", "June", "July", "August",
		       "September", "October", "November", "December"]
    in
	get_nth(months_names, #2 date) ^ " " ^ 
	Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, array: int list) =
    let
	fun get_idx(s: int, ar: int list, count) = 
	    if s <= (hd ar) then count
	    else get_idx(s - (hd ar), tl ar, count + 1)
    in
	get_idx(sum, array, 0)
    end

fun what_month(day: int) = 
    let 
	val day_of_months = [31, 28, 31, 30,
			     31, 30, 31, 31,
			     30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, day_of_months)
    end

fun month_range(day1: int, day2: int) = 
    if day1 > day2 then [] else what_month(day1)::month_range(day1 + 1, day2)
    
fun oldest(date_list: (int*int*int) list) = 
    if null date_list then NONE
    else 
	let
	    fun oldest_date (ans: (int*int*int), dates: (int*int*int) list) = 
		if null dates then ans
		else if is_older(ans, hd dates) then oldest_date(ans, tl dates)
		else oldest_date(hd dates, tl dates)
	in
	    SOME(oldest_date(hd date_list, tl date_list))
	end
fun number_in_months_challenge(dates: (int*int*int) list, months: int list) = 
    let
	fun has(array: int list, e: int) = 
	    if null array then false
	    else if hd array = e then true
	    else has(tl array, e)
	fun unique_array(array: int list, res: int list) = 
	    if null array orelse has(res, hd array) then res
	    else unique_array(tl array, (hd array)::res)
	val uniq_months = unique_array(months, [])
    in
	number_in_months(dates, uniq_months)
    end
fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) = 
    let
	fun has(array: int list, e: int) =
	    if null array then false
	    else if hd array = e then true
	    else has(tl array, e)
	fun unique_array(array: int list, res: int list) = 
	    if null array orelse has(res, hd array) then res
	    else unique_array(tl array, (hd array)::res)
	val uniq_months = unique_array(months, [])
    in
	dates_in_months(dates, uniq_months)
    end
    
fun reasonable_date(date: int*int*int) = 
    if #1 date <= 0 then false
    else if #2 date <= 0 orelse #2 date > 12 then false
    else 
	let 
	    val day_of_months = [31, 28, 31, 30,
				 31, 30, 31, 31,
				 30, 31, 30, 31]
	    fun is_leapyear(year: int) =  
		if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0) then true else false
	    fun get_nth(array: int list, idx: int) = 
		if idx = 1 then hd array else get_nth(tl array, idx - 1)
	    val max_day = if (#2 date <> 2) then get_nth(day_of_months, #2 date)
			  else if is_leapyear(#1 date) then 29
			  else 28
	in
	    #3 date <= max_day
	end			    
