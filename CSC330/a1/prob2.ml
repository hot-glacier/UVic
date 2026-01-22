(* CSC330: Assignment 1, Problem 2
 * Ryan Glaeser 
 * V00832892 *)


(* Custom country type used in get_records function *)
 type country = {id : string; name : string; rates : float option list}

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
 * the number of available inflation rates recorded *)
let avail (record : country) =
  let rec counter count rates =
    match rates with
    | [] -> count
    | Some _ :: rest -> counter (count + 1) rest
    | None :: rest -> counter count rest
  in
  counter 0 record.rates

(* This function takes in a country record and returns
 * the most recent year that has an inflation rate recorded *)
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
    | None :: rest -> find_minmax (year + 1) min max rest
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

(* This function takes a list of countries and an ID string and returns
 * a summary of the country's inflation rates *)
let summarize ((countries : country list), (id : string)) =
  let rec find_country records =
    match records with
    | [] -> "Cannot find " ^ id
    | record :: rest ->
      if record.id = id then
        let available = avail record in
        let last_year =
          match last record with
          | None -> ""
          | Some (year, rate) -> string_of_int year ^ " with rate of " ^ string_of_float rate ^ "%"
        in
        let (min_rate, max_rate) = minmax record in
        let min =
          match min_rate with
          | None -> ""
          | Some (year, rate) -> string_of_int year ^ " with rate of " ^ string_of_float rate ^ "%"
        in
        let max =
          match max_rate with
          | None -> ""
          | Some (year, rate) -> string_of_int year ^ " with rate of " ^ string_of_float rate ^ "%"
        in
        if (available = 0) then
          "Country: " ^ record.name ^ " (" ^ record.id ^ ")" ^ "\n" ^
          "Records available: " ^ string_of_int available ^ " years"

        else
          "Country: " ^ record.name ^ " (" ^ record.id ^ ")" ^ "\n" ^
          "Records available: " ^ string_of_int available ^ " years" ^ "\n" ^
          "Last record: " ^ last_year ^ "\n" ^
          "Lowest rate: " ^ min ^ "\n" ^
          "Highest rate: " ^ max
      else
        find_country rest
  in
  find_country countries

(* Bonus task: This function takes in a separating string and a list of strings
 * and returns a string of the list elements separated by the first argument *)
 let concat ((sep : string), (strings : string list)) =
  let rec iterate acc rem_strings =
    match rem_strings with
    | [] -> acc
    | "" :: rest -> iterate acc rest
    | s :: rest -> 
      if acc = "" then iterate s rest
      else iterate (acc ^ sep ^ s) rest
  in
  iterate "" strings