(* Testing *)
open Prob1

let test_is_older () =
assert (is_older ((1990, 1, 1), (1995, 1, 15)) = true);
print_string ("is_older test 1: (1990, 1, 1), (1995, 1, 15)) = true\n");
assert (is_older ((1996, 2, 21), (1996, 2, 20)) = false);
print_string ("is_older test 1: (1996, 2, 21), (1996, 2, 20)) = false\n")
let _ = test_is_older () 

let test_days_in_month () =
assert (days_in_month(2000, 1) = Some 31);
print_string ("days_in_month test 1: (2000, 1) = Some 31\n");
assert (days_in_month(2001, 2) = Some 28);
print_string ("days_in_month test 2: (2001, 2) = Some 28\n");
assert (days_in_month(2000, 2) = Some 29);
print_string ("days_in_month test 3: (2000, 2) = Some 29\n");
assert (days_in_month(2000, 15) = None);
print_string ("days_in_month test 3: (2000, 15) = None\n")
let _ = test_days_in_month ()

let test_dates_in_month () =
assert (dates_in_month(2000, 1) = Some [
  (2000, 1, 1); (2000, 1, 2); (2000, 1, 3); (2000, 1, 4); (2000, 1, 5);
   (2000, 1, 6); (2000, 1, 7); (2000, 1, 8); (2000, 1, 9); (2000, 1, 10);
    (2000, 1, 11); (2000, 1, 12); (2000, 1, 13); (2000, 1, 14); (2000, 1, 15);
     (2000, 1, 16); (2000, 1, 17); (2000, 1, 18); (2000, 1, 19); (2000, 1, 20);
      (2000, 1, 21); (2000, 1, 22); (2000, 1, 23); (2000, 1, 24); (2000, 1, 25);
       (2000, 1, 26); (2000, 1, 27); (2000, 1, 28); (2000, 1, 29); (2000, 1, 30);
        (2000, 1, 31)]);
print_string ("dates_in_month test 3: (2000, 1) = Some [(2000, 1, 1); ...; (2000, 1, 31)]\n");
assert (dates_in_month(2000, 15) = None);
print_string ("dates_in_month test 3: None\n")
let _ = test_dates_in_month ()

let test_num_of_days () =
assert (num_of_days(2000, 1, 1) = Some 1);
print_string ("num_of_days test 1: (2000, 1, 1) = Some 1\n");
assert (num_of_days(2000, 1, 30) = Some 30);
print_string ("num_of_days test 2: (2000, 1, 30) = Some 30\n");
assert (num_of_days(2000, 6, 15) = Some 167);
print_string ("num_of_days test 3: (2000, 6, 15) = Some 365\n");
assert (num_of_days(2001, 12, 31) = Some 365);
print_string ("num_of_days test 4: (2001, 12, 31) = Some 365\n")
let _ = test_num_of_days ()

let test_nth_day () =
assert (nth_day(2000, 0) = None);
print_string ("nth_day test 1: (2000, 0) = None\n");
assert (nth_day(2000, 1) = Some (2000, 1, 1));
print_string ("nth_day test 2: (2000, 1) = Some (2000, 1, 1)\n");
assert (nth_day(2000, 31) = Some (2000, 1, 31));
print_string ("nth_day test 3: (2000, 31) = Some (2000, 1, 31)\n");
assert (nth_day(2000, 32) = Some (2000, 2, 1));
print_string ("nth_day test 4: (2000, 32) = Some (2000, 2, 1)\n");
assert (nth_day(2000, 366) = Some (2000, 12, 31));
print_string ("nth_day test 5: (2000, 366) = Some (2000, 12, 31)\n");
assert (nth_day(2001, 365) = Some (2001, 12, 31));
print_string ("nth_day test 1: (2001, 365) = Some (2001, 12, 31)\n")
let _ = test_nth_day ()