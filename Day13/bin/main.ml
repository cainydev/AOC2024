open Batteries
open Line_oriented

let extract_nums s =
  let filtered = String.filter (fun c -> Char.is_digit c || c = ',') s in
  let nums = String.split_on_char ',' filtered in
  if List.length nums <> 2
  then failwith "Only can do pairs"
  else (float_of_string @@ List.at nums 0, float_of_string @@ List.at nums 1)

let parse_input input = List.filter_map (fun lines ->
  match lines with
  | [a; b; p; _] ->
    let button_a = extract_nums a in
    let button_b = extract_nums b in
    let target = extract_nums p in
    Some (button_a, button_b, target)
  | _ -> None
  ) @@ List.ntake 4 input

(* Use Cramers Rule to solve equations *)
let solve_eq (ax, ay) (bx, by) (px, py) =
  let a = (px *. by -. py *. bx) /. (ax *. by -. ay *. bx) in
  let b = (ax *. py -. ay *. px) /. (ax *. by -. ay *. bx) in
  if a >= 0.0 && b >= 0.0 && Float.floor a = a && Float.floor b = b
  then Some (int_of_float a, int_of_float b)
  else None

let part1 =
  lines_of_file "input.txt"
  |> parse_input
  |> List.filter_map (Tuple3.uncurry solve_eq)
  |> List.map (fun (a, b) -> a * 3 + b)
  |> List.sum

let part2 =
  lines_of_file "input.txt"
  |> parse_input
  |> List.map (Tuple3.map3 (Tuple2.mapn ((+.) 10000000000000.)))
  |> List.filter_map (Tuple3.uncurry solve_eq)
  |> List.map (fun (a, b) -> a * 3 + b)
  |> List.sum

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
