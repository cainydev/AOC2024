open Batteries

let read_lines file =
  let ic = open_in file in
  let rec read_lines_rec acc = 
    try
      let line = input_line ic in
      read_lines_rec (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in read_lines_rec []

let extract_matches regex input =
  let r = Str.regexp regex in
  let rec extract acc pos =
    try
      let _ = Str.search_forward r input pos in
      let matched = Str.matched_string input in
      extract (matched :: acc) (Str.match_end ())
    with Not_found -> List.rev acc
  in
  extract [] 0

let part1 =
  read_lines "input.txt"
  |> String.concat ""
  |> extract_matches {|mul([0-9]+,[0-9]+)|}
  |> List.concat_map (extract_matches {|[0-9]+,[0-9]+|})
  |> List.map (fun s ->
      String.split_on_char ',' s
      |> List.map (int_of_string)
      |> List.fold_left ( * ) 1)
  |> List.fold_left ( + ) 0

let filter_muls ops =
  let _,filtered = List.fold_left (fun (enabled, acc) op ->
    match op with
    | "do()" -> (true, acc)
    | "don't()" -> (false, acc)
    | mul -> if enabled then (enabled, mul::acc) else (enabled, acc)
  ) (true, []) ops in List.rev filtered


let part2 = read_lines "input.txt"
  |> String.concat ""
  |> extract_matches {|mul([0-9]+,[0-9]+)\|don't()\|do()|}
  |> filter_muls
  |> List.concat_map (extract_matches {|[0-9]+,[0-9]+|})
  |> List.map (fun s ->
      String.split_on_char ',' s
      |> List.map (int_of_string)
      |> List.fold_left ( * ) 1)
  |> List.fold_left ( + ) 0

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
