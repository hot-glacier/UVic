(* Testing *)
open Prob2

let data = "Canada,CAN,0.1,,2.1\nUnited States,USA,1.1,2.1,1.1\nUnknown,UNK,,,"

let records = get_records data
let () =
  assert (records = 
  [ {id = "CAN"; name = "Canada"; rates = [Some 0.1; None; Some 2.1]}
  ; {id = "USA"; name = "United States"; rates = [Some 1.1; Some 2.1; Some 1.1]}
  ; {id = "UNK"; name = "Unknown"; rates = [None; None; None]} ]);
print_string("get_records test 1: Pass\n")

let () =
  assert (avail (List.hd records) = 2);
print_string("avail test 1: Pass\n");
  assert (avail ({id = "UNK"; name = "Unknown"; rates = [None; None; None]}) = 0);
print_string("avail test 2: Pass\n")

let () =
  assert (last (List.hd records) = Some (1962, 2.1));
print_string("last test 1: Pass\n");
  assert (last ({id = "UNK"; name = "Unknown"; rates = [None; None; None]}) = None);
print_string("last test 2: Pass\n")

let () =
  assert (minmax (List.hd records) = (Some (1960, 0.1), Some (1962, 2.1)));
print_string("minmax test 1: Pass\n");
assert (minmax ({id = "UNK"; name = "Unknown"; rates = [None; None; None]}) = (None, None));
print_string("minmax test 2: Pass\n")




let read_file path =
let fp = open_in path in
let s = really_input_string fp (in_channel_length fp) in
close_in fp;
s

let records_v2 = get_records (read_file "csc330_a1.csv")
let () =
  let x = summarize (records_v2, "CAN") in
  print_string ("\n" ^ x ^ "\n\n");

let () =
  let x = summarize (records_v2, "AND") in
  print_string ("\n" ^ x ^ "\n\n")
in

let y = concat (", ", ["a"; ""; "b"]) in
print_string ("concat test 1: " ^ y ^ "\n");

let y = concat (", ", ["Bees"; ""; "shouldn't"; ""; ""; "be"; "able"; "to fly"; ""]) in
print_string ("concat test 2: " ^ y ^ "\n");