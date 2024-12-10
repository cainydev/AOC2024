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

let to_matrix (lines: string list): char array array =
  lines 
  |> List.map (Array.of_list % String.to_list)
  |> Array.of_list

let step matrix (_outside, (px, py), (dx, dy)) =
  let (w, h) = (Array.length matrix.(0), Array.length matrix) in
  let (nx, ny) = (px + dx, py + dy) in
  if nx < 0 || nx >= w || ny < 0 || ny >= h
  then true, (nx, ny), (dx, dy)
  else match matrix.(ny).(nx) with
    | '#' -> false, (px, py), (-dy, dx)
    | _ -> false, (nx, ny), (dx, dy)

let find_first_in_matrix elem matrix =
  let (w, h) = (Array.length matrix.(0), Array.length matrix) in
  let rec find x y =
    if y >= h then raise Not_found
    else if x >= w then find 0 (y + 1)
    else if matrix.(y).(x) = elem then (x, y)
    else find (x + 1) y
  in
  find 0 0

let find_visited_squares m =
  let start_pos = find_first_in_matrix '^' m in
  let start_state = false, start_pos, (0, -1) in
  let rec step_and_collect pos_set state = 
    let (outside, pos, dir) = step m state in
    if outside
    then pos_set
    else step_and_collect (Set.add pos pos_set) (outside, pos, dir)
  in
    step_and_collect (Set.singleton start_pos) start_state
    |> Set.to_list

let part1 =
  let m = read_lines "input.txt" |> to_matrix in
  List.length (find_visited_squares m)

let escapes m start_state =
  let seen_states = Hashtbl.create 100 in
  let rec simulate ((x, y), (dx, dy)) =
    let (outside, np, nd) = step m ((), (x, y), (dx, dy)) in
    if Hashtbl.mem seen_states (np, nd) then false
    else if outside then true
    else begin
      Hashtbl.add seen_states (np, nd) true;
      simulate (np, nd)
    end
  in simulate start_state

let replace_in_matrix matrix (x, y) new_char =
  let new_matrix = Array.map Array.copy matrix in
  new_matrix.(y).(x) <- new_char;
  new_matrix

let part2 =
  let m = read_lines "input.txt" |> to_matrix in
  let start_pos = find_first_in_matrix '^' m in
  let start_state = start_pos, (0, -1) in
  let visited = find_visited_squares m in
  List.count_matching (fun (x, y) ->
    not (escapes (replace_in_matrix m (x, y) '#') start_state) 
  ) visited

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
