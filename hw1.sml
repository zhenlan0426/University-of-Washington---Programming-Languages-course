fun is_older(date1: int*int*int, date2: int*int*int)=
	let fun ToDay(date: int*int*int)= #1 date *365 + #2 date * 30 + #3 date
	in ToDay(date1) < ToDay(date2)
	end

fun number_in_month(dateList: (int*int*int) list, month:int)=
	if null dateList
	then 0
	else let fun IsInMonth(date:int*int*int, month:int)=
				 if #2 date=month
				 then 1
				 else 0
		 in IsInMonth(hd dateList,month) + number_in_month(tl dateList, month)
		 end


fun number_in_months(dateList: (int*int*int) list, monthList:int list)=
	if null monthList
	then 0
	else number_in_month(dateList, hd monthList) + number_in_months(dateList, tl monthList)


fun dates_in_month(dateList: (int*int*int) list, month:int)=
	if null dateList
	then []
	else if #2 (hd dateList) = month
		 then hd dateList :: dates_in_month(tl dateList, month)
		 else dates_in_month(tl dateList, month)

fun dates_in_months(dateList: (int*int*int) list, monthList:int list)=
	if null monthList
	then []
	else dates_in_month(dateList,hd	monthList) @ dates_in_months(dateList,tl monthList)

fun get_nth(strList:string list, n:int)=
	if n = 1
	then hd strList
	else get_nth(tl strList, n-1)

fun data_to_string(date:int*int*int)=
	let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

fun number_before_reaching_sum(sum : int, intList : int list)=
	if hd intList >= sum
	then 0
	else 1 + number_before_reaching_sum(sum-hd intList, tl intList)


fun what_month(day : int)=
	let val daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in 1 + number_before_reaching_sum(day,daysInMonth)
	end

fun month_range(day1:int, day2:int)=
	if day1>day2
	then []
	else what_month(day1) :: month_range(day1+1,day2)

fun oldest(dateList: (int*int*int) list)=
	if null dateList
	then NONE
	else let fun oldest_woNONE(dateList: (int*int*int) list)=
			     if null (tl dateList)
			     then hd dateList
			     else let val tlOldest = oldest_woNONE(tl dateList)
			     	  in if is_older(hd dateList, tlOldest)
			     	  	 then hd dateList
			     	  	 else tlOldest
			     	  end
		 in SOME (oldest_woNONE dateList)
		 end



