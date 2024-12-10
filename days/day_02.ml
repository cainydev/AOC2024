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

let parse_input (lines: string list): (int list) list = lines
|> List.map (List.map int_of_string % String.split_on_char ' ')

let rec is_valid asc = function
  | [] -> true
  | [_] -> true
  | (x::y::xs) -> let d = abs (x - y) in
    d >= 1 && d <= 3 && asc = (x < y) && is_valid asc (y::xs)

let part1 = read_lines "input.txt"
  |> parse_input
  |> List.filter (fun seq -> is_valid true seq || is_valid false seq)
  |> List.length

let alternatives xs = (0 -- (List.length xs - 1))
  |> Enum.map (fun i -> List.filteri (fun n _ -> n <> i) xs)
  |> List.of_enum

let part2 = read_lines "input.txt"
  |> parse_input
  |> List.filter (fun seq ->
      let possibilities = (seq::alternatives seq) in
      List.exists (fun l -> is_valid true l || is_valid false l) possibilities)
  |> List.length

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
