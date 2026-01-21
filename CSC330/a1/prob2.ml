(* CSC330: Assignment 1, Problem 2
 * Ryan Glaeser 
 * V00832892 *)



(* Custom country type used in get_records function *)
 type country = {id : string; name : string; rates : float option list}

(* Read in file *)
let read_file path =
  let fp = open_in path in
  let s = really_input_string fp (in_channel_length fp) in
  close_in fp;
  s

(* This function takes in a string and returns a formatted
 * list of countries in the form {name; id; rates_list} *)
let get_records (csv_contents : string) =
  let lines = String.split_on_char '\n' csv_contents in
  let rec parse_rates ss =
    match ss with
    | [] -> []
    | ss :: ss' ->
      let rate =
        if ss = "" then None
        else Float.of_string_opt ss
        in
        rate :: parse_rates ss'
  in

  let rec parse_lines ss =
    match ss with
    | [] -> []
    | line :: remaining -> 
      match String.split_on_char ',' line with
      | name :: id :: rates_list ->
        {name; id; rates = parse_rates rates_list} :: parse_lines remaining
      | _ -> parse_lines remaining
  in
  parse_lines lines

(* This function takes in a country record and returns
 * a count of the available inflation rates recorded *)
let avail (record : country) =
  let rec counter count rates =
    match rates with
    | [] -> count
    | Some _ :: rest -> counter (count + 1) rest
    | None :: rest -> counter count rest
  in
  counter 0 record.rates

(* This function takes in a country record and returns
 * the most recent year that has an inflation rate recorded*)
let last (record : country) =
  let rec find_last year latest rates =
    match rates with
    | [] -> latest
    | Some rate :: rest -> 
        find_last (year + 1) (Some (year, rate)) rest
    | None :: rest -> 
        find_last (year + 1) latest rest
    in
    find_last 1960 None record.rates

(* This function takes in a country record and returns
 * the lowest and highest inflation rates, and their years *)
 let minmax (record : country) =
  let rec find_minmax year min max rates =
    match rates with
    | [] -> (min, max)
    | None :: rest ->
      find_minmax (year + 1) min max rest
    | Some rate :: rest ->
      let new_min =
        match min with
        | None -> Some (year, rate)
        | Some (old_year, old_rate) when rate < old_rate -> Some (year, rate)
        | _ -> min
      in
      let new_max =
        match max with
        | None -> Some (year, rate)
        | Some (old_year, old_rate) when rate > old_rate -> Some (year, rate)
        | _ -> max
      in
      find_minmax (year + 1) new_min new_max rest
  in
  find_minmax 1960 None None record.rates

(* This function *)
let summarize ((countries : country list), (id : string)) =
  