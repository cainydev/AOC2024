open Batteries

let prefix_table = Hashtbl.create 1_000_000

let rec is_prefix_of str prefix =
  if Hashtbl.mem prefix_table (str, prefix) then Hashtbl.find prefix_table (str, prefix) else
  match str, prefix with
  | rest, [] -> Hashtbl.add prefix_table (str, prefix) (Some rest); Some rest
  | [], _ -> Hashtbl.add prefix_table (str, prefix) None; None
  | x::xs, y::ys when x = y -> is_prefix_of xs ys
  | _ -> Hashtbl.add prefix_table (str, prefix) None; None

let is_possible_table = Hashtbl.create 1_000_000

let rec is_possible towels pattern =
  if pattern = [] then true else
  let possible_rests = List.filter_map (is_prefix_of pattern) towels in
  List.filter (fun rest ->
    let pos = if Hashtbl.mem is_possible_table rest
      then Hashtbl.find is_possible_table rest
      else
        let p = is_possible towels rest in
        Hashtbl.add is_possible_table rest p; p
    in pos
  ) possible_rests
  |> (not % List.is_empty)

let count_possibilities_table = Hashtbl.create 1_000_000

let rec count_possibilities towels pattern =
  if pattern = [] then 1 else
  let possible_rests = List.filter_map (is_prefix_of pattern) towels in
  List.map (fun rest ->
    let count = if Hashtbl.mem count_possibilities_table rest
      then Hashtbl.find count_possibilities_table rest
      else
        let c = count_possibilities towels rest in
        Hashtbl.add count_possibilities_table rest c; c
    in count
  ) possible_rests
  |> List.sum

let part1 =
  let towels, patterns = match Line_oriented.lines_of_file "input.txt" with
    | [] -> failwith "Input file is empty"
    | [_] -> failwith "Input file has only one line"
    | towels::_::patterns -> towels, patterns
  in
  let towels = String.split_on_string towels ~by:", " |> List.map (String.to_list % String.trim) in
  let patterns = List.map (String.to_list % String.trim) patterns in
  
  Printf.printf "\nTowels: %s\nPatterns: %s\n"
    (String.concat ", " (List.map String.of_list towels))
    (String.concat ", " (List.map String.of_list patterns));

  List.filter (fun p ->
    let pos = is_possible towels p in
    Printf.printf "Pattern: %s, Possible: %b\n%!" (String.of_list p) pos; pos
  ) patterns |> List.length

let part2 =
  let towels, patterns = match Line_oriented.lines_of_file "input.txt" with
    | [] -> failwith "Input file is empty"
    | [_] -> failwith "Input file has only one line"
    | towels::_::patterns -> towels, patterns
  in
  let towels = String.split_on_string towels ~by:", " |> List.map (String.to_list % String.trim) in
  let patterns = List.map (String.to_list % String.trim) patterns in
  
  Printf.printf "\nTowels: %s\nPatterns: %s\n"
    (String.concat ", " (List.map String.of_list towels))
    (String.concat ", " (List.map String.of_list patterns));

  List.map (fun p ->
    let count = count_possibilities towels p in
    Printf.printf "Pattern: %s, Possibilities: %i\n%!" (String.of_list p) count; count
  ) patterns |> List.sum

let () =
  Printf.printf "\nPart 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
