open Solution_runner

let run_day day =
  let input_file = Printf.sprintf "input/input_%02d.txt" day in
  match day with
  | 1 ->
      let module Runner = Make (Day_01) in
      Runner.run input_file
  | 2 ->
      let module Runner = Make (Day_02) in
      Runner.run input_file
  | _ -> Printf.printf "Solution for day %d is not implemented.\n" day

let () =
  match Array.to_list Sys.argv with
  | [_; day_str] ->
      let day = int_of_string day_str in
      run_day day
  | _ -> Printf.printf "Usage: ./aoc_project <day>\n"
