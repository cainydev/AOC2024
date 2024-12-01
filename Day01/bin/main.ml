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

let part1 = 
  read_lines "input.txt"
  |> List.map (fun s -> (String.sub s 0 5, String.sub s 8 5))
  |> List.map (fun (l, r) -> (int_of_string l, int_of_string r))
  |> List.split
  |> (fun (l, r) -> (List.sort compare l, List.sort compare r))
  |> fun (l, r) -> List.map2 (fun x y -> abs (x - y)) l r
  |> List.fold_left (+) 0

let part2 =
  let (l, r) =
    read_lines "input.txt"
    |> List.map (fun s -> (String.sub s 0 5, String.sub s 8 5))
    |> List.map (fun (l, r) -> (int_of_string l, int_of_string r))
    |> List.split
  in
  let occurrences elem xs =
    xs |> List.filter (fun x -> x = elem) |> List.length
  in
  l
  |> List.map (fun target -> target * (occurrences target r))
  |> List.fold_left (+) 0 

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
