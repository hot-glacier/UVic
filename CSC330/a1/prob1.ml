(* CSC330: Assignment 1
 * Ryan Glaeser 
 * V00832892 *)


(* Given functions for assignment *) 
let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let thd3 (_, _, x) = x


(* Number of days in each month depending on leap year *)
let non_leap = [31;28;31;30;31;30;31;31;30;31;30;31] 
let leap = [31;29;31;30;31;30;31;31;30;31;30;31] 


(* Function for getting the nth value of the above lists *)
let rec nth ((xs: int list), (n : int)) =
  if n = 0 then List.hd xs
  else nth (List.tl xs, n - 1)


(* Function to test if a date falls during a leap year *)
let is_leap (year : int) =
  if (year mod 4 = 0) then
      if (year mod 100 = 0) then
        if (year mod 400 = 0) then true
        else false
      else true
    else false

(* Function to make a list of triples for the dates_in_month function *)
let make_list ((date : int * int), (n : int)) =
  let rec build_list (current) =
    if current > n then []
    else (fst date, snd date, current) :: build_list (current+1)
  in
  Some (build_list 1)


(* This function tests if the first given date is older than the second given date *)
let is_older ((tr1 : int * int * int), (tr2 : int * int * int)) =
  if fst3 tr1 = fst3 tr2 then
    if snd3 tr1 = snd3 tr2 then
      if thd3 tr1 = thd3 tr2 then false
      else thd3 tr1 < thd3 tr2
    else snd3 tr1 < snd3 tr2
  else fst3 tr1 < fst3 tr2


(* This function returns how many days are in a given month, in a given year *)
let days_in_month (date : int * int) =
  if (fst date < 1) || (fst date > 3000) || (snd date < 1) || (snd date > 12) then None
  else
    if is_leap(fst date) then Some (nth (leap, snd date-1))
    else Some (nth (non_leap, snd date-1))


(* This function returns a list of triples containing the dates in the given month *)
let dates_in_month (date : int * int) =
  if (fst date < 1) || (fst date > 3000) || (snd date < 1) || (snd date > 12) then None
  else
    if is_leap(fst date) then make_list(date, (Option.get(days_in_month(date))))
    else make_list(date, (Option.get(days_in_month(date))))

(* This function returns the number of days from the beginning
 * of that year until (and including) the given date *)
let num_of_days (date : int * int * int) =
  if (fst3 date < 1) || (fst3 date > 3000) || (snd3 date < 1) || (snd3 date > 12) then None
  else
    if (thd3 date < 1) || (thd3 date > Option.get(days_in_month(fst3 date, snd3 date))) then None
    else 
      let rec count_days (current) =
        if current > (snd3 date) then 0
        else 
          if (current = snd3 date) then thd3 date
          else Option.get(days_in_month(fst3 date, current)) + count_days(current + 1)
      in
      Some (count_days 1)


(* This function returns a date given a number that represents the
 * n-th day of the year *)
let nth_day ((year : int), (n : int)) =
  if (year < 1) || (year > 3000) || (n < 1) || ((is_leap(year)) && (n > 366)) || (not(is_leap(year)) && (n > 365)) then None
  else
    let rec find_date((month: int), (remaining_days : int)) =
      if (remaining_days <= Option.get(days_in_month(year, month))) then (year, month, remaining_days)
      else
        find_date ((month + 1), (remaining_days - Option.get(days_in_month(year,month))))
    in
    Some (find_date (1, n))