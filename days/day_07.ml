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

let parse_equation line =
  let split = String.split_on_string ~by:": " line in
  let nums =
    String.split_on_char ' ' (List.at split 1)
    |> List.map int_of_string
  in
  (int_of_string (List.at split 0), nums)

let eq_is_possible (r, nums) =
  let rec check acc = function
    | [] -> acc = r
    | (y::ys) -> check (acc + y) ys || check (acc * y) ys
  in check (List.hd nums) (List.tl nums)
  
let concat_ints a b =
  if b < 10 then a * 10 + b
  else if b < 100 then a * 100 + b
  else if b < 1000 then a * 1000 + b
  else
    let digits_b = Float.floor (float_of_int b |> log10) +. 1. in
    a * int_of_float (10. ** digits_b) + b

let eq_is_possible_with_concat (r, nums) =
  let rec check acc = function
    | [] -> acc = r
    | (y::ys) ->
          if acc > r then false else
          check (acc + y) ys
          || check (acc * y) ys
          || check (concat_ints acc y) ys
  in check (List.hd nums) (List.tl nums)

let () =
  let equations = read_lines "input.txt" |> List.map parse_equation in
  let pos_for_1 = equations |> List.filter eq_is_possible in
  let to_be_check_for_2 = List.filter (fun x -> not (List.mem x pos_for_1)) equations in
  let pos_for_2 = to_be_check_for_2 |> List.filter eq_is_possible_with_concat in
  let result_1 = (List.map (Tuple2.first) pos_for_1 |> List.sum) in
  let result_2 = (List.map (Tuple2.first) pos_for_2 |> List.sum) in
  Printf.printf "Part 1: %i\n" result_1;
  Printf.printf "Part 2: %i\n" (result_1 + result_2); 
