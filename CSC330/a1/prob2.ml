let read_file path =
  let fp = open_in path in
  let s = really_input_string fp (in_channel_length fp) in
  close_in fp;
  s

let filename = "csc330_a1.csv"
let content = read_file filename
let () = print_string content