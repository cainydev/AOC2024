open Batteries

let parse_tuple str =
  match String.split_on_char ',' str with
  | [l; r] -> (int_of_string l, int_of_string r)
  | _ -> failwith "Parse error"
  
let parse_robot line =
  match String.split_on_char ' ' line with
  | [p; v] ->
      let (pt, vt) = (String.lchop ~n:2 p, String.lchop ~n:2 v) in
      (parse_tuple pt, parse_tuple vt)
  | _ -> failwith "Parse error"

let simulate_robot n (w, h) ((px, py), (vx, vy)) =
  let wrap_x = ((px + n * vx) mod w + w) mod w in
  let wrap_y = ((py + n * vy) mod h + h) mod h in
  (wrap_x, wrap_y), (vx, vy)

let get_quadrant (w, h) (x, y) =
  let max_x, min_x = w / 2, int_of_float @@ Float.ceil @@ (float_of_int w) /. 2.0 in
  let max_y, min_y = h / 2, int_of_float @@ Float.ceil @@ (float_of_int h) /. 2.0 in 
  if x < max_x && y < max_y then Some 0 (* tl *)
  else if x >= min_x && y < max_y then Some 1 (* tr *)
  else if x < max_x && y >= min_y then Some 2 (* bl *)
  else if x >= min_x && y >= min_y then Some 3 (* br *)
  else None

let safety_factor (w, h) robots =
  robots
  |> List.fold_left (fun acc (pos, _v) ->
      match get_quadrant (w, h) pos with
      | Some 0 -> Tuple4.map1 ((+) 1) acc
      | Some 1 -> Tuple4.map2 ((+) 1) acc
      | Some 2 -> Tuple4.map3 ((+) 1) acc
      | Some 3 -> Tuple4.map4 ((+) 1) acc
      | Some _ -> failwith "Somethings wrong"
      | None -> acc
  ) (0, 0, 0, 0)
  |> (fun (a, b, c, d) -> a * b * c * d)

let part1 =
  let (w, h) = (101, 103) in
  let inputfile = "input.txt" in

  Line_oriented.lines_of_file inputfile
  |> List.map parse_robot
  |> List.map @@ simulate_robot 100 (w, h)
  |> safety_factor (w, h)
  
let part2 = 
  let (w, h) = (101, 103) in
  let inputfile = "input.txt" in

  let robots =
    Line_oriented.lines_of_file inputfile
    |> List.map parse_robot
  in
  let measurements = Array.make 10001 0 in

  for seconds = 0 to 10000 do
    measurements.(seconds) <-
      List.map (simulate_robot seconds (w, h)) robots |> safety_factor (w, h);
  done;

  Array.fold_lefti (fun (min, mini) second measurement ->
    if measurement < min
    then (measurement, second)
    else (min, mini)
  ) (Int.max_num, 0) measurements
  |> snd

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
