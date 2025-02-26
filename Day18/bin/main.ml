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

let part1 = read_lines "input.txt" |> List.length

let part2 = 0

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
