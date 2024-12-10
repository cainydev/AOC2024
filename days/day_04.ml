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

let count_xmas = List.length % extract_matches {|XMAS|}
let count_xmas_list xs = List.map count_xmas xs |> List.fold_left (+) 0

let diagonals lines =
  let matrix = List.map String.to_seq lines |> List.map Array.of_seq |> Array.of_list in
  let width = Array.length matrix.(0) in
  let height = Array.length matrix in

  let rec collect_diagonal x y dx dy =
    if x >= width || y >= height || x < 0 || y < 0 then []
    else matrix.(y).(x) :: collect_diagonal (x + dx) (y + dy) dx dy
  in

  let tl_to_br_starts =
    let xstarts = List.init width (fun x -> (x, 0)) |> List.tl in
    let ystarts = List.init height (fun y -> (0, y)) in
    xstarts @ ystarts
  in 
  let tl_to_br_diags =
    List.map (fun (x, y) -> collect_diagonal x y 1 1) tl_to_br_starts
  in

  let bl_to_tr_starts =
    let xstarts = List.init width (fun x -> (x, height - 1)) |> List.tl in
    let ystarts = List.init height (fun y -> (0, y)) in
    xstarts @ ystarts
  in
  let bl_to_tr_diags =
    List.map (fun (x, y) -> collect_diagonal x y 1 (-1)) bl_to_tr_starts
  in

  let all_diags = tl_to_br_diags @ bl_to_tr_diags in
  List.map (fun chars -> String.of_seq (List.to_seq chars)) all_diags

let part1 =
  let input = read_lines "input.txt" in
  let input_rev = List.map (String.rev) input in
  let input_vert = List.map (String.to_list) input
    |> List.transpose
    |> List.map (String.of_list) in
  let input_vert_rev = List.map (String.rev) input_vert in
  let diagonals_normal = diagonals input in
  let diagonals_rev = diagonals input_rev in
  count_xmas_list input +
  count_xmas_list input_rev +
  count_xmas_list input_vert +
  count_xmas_list input_vert_rev +
  count_xmas_list diagonals_normal +
  count_xmas_list diagonals_rev

let all_pos_of needle matrix =
  Array.fold_lefti (fun acc1 y line ->
    Array.fold_lefti (fun acc2 x elem ->
      if elem = needle then (x, y) :: acc2 else acc2
    ) acc1 line
  ) [] matrix

let part2 =
  let matrix = read_lines "input.txt"
  |> List.map (Array.of_list % String.to_list)
  |> Array.of_list in
  let width = Array.length matrix.(0) in
  let height = Array.length matrix in
  all_pos_of 'A' matrix
  |> List.filter (fun (x, y) ->
    if x = 0 || x = (width - 1) || y = 0 || y = (height - 1)
    then false else
    let tl = int_of_char matrix.(y-1).(x-1) in
    let br = int_of_char matrix.(y+1).(x+1) in
    let bl = int_of_char matrix.(y-1).(x+1) in
    let tr = int_of_char matrix.(y+1).(x-1) in
    ((tl + br) = 160) && ((bl + tr) = 160))
  |> List.length

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
