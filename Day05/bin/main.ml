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

let parse_rules input: (int * int) list = 
  List.take_while (Fun.flip String.contains '|') input 
  |> List.map (String.split_on_char '|')
  |> List.map (function
    | [this; beforethat] -> int_of_string this, int_of_string beforethat
    | _ -> raise (Failure "Parse error"))

let parse_updates input: int list list =
  List.drop_while (not % Fun.flip String.contains ',') input 
  |> List.map (List.map int_of_string % String.split_on_char ',')

let update_is_valid rules update =
  List.for_all (fun (this, beforethat) ->
    let index1 = List.index_of this update in
    let index2 = List.index_of beforethat update in
    match index1, index2 with Some i, Some j -> i < j | _ -> true
  ) rules

let part1 =
  let lines = read_lines "input.txt" in
  let rules = parse_rules lines in
  let updates = parse_updates lines in
  List.filter (update_is_valid rules) updates
  |> List.map (fun valid_rule ->
      let middle_index = (List.length valid_rule / 2) in
      List.at valid_rule middle_index)
  |> List.sum

let sort_function_by_rules rules = fun a b ->
  match List.find_opt (fun r -> r = (a, b) || r = (b, a)) rules with
  | Some (this, _) -> if a = this then -1 else 1
  | None -> 0

let part2 = 
  let lines = read_lines "input.txt" in
  let rules = parse_rules lines in
  let updates = parse_updates lines in
  List.filter (not % update_is_valid rules) updates
  |> List.map (List.sort (sort_function_by_rules rules))
  |> List.map (fun valid_rule ->
      let middle_index = (List.length valid_rule / 2) in
      List.at valid_rule middle_index)
  |> List.sum

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
