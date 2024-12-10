open Batteries
open Line_oriented

let file_to_grid filename =
  let lines =
    lines_of_file filename
    |> List.map (Array.of_list % String.to_list)
    |> Array.of_list
  in
  let h, w = Array.length lines, Array.length (lines.(0)) in
  Grid.init h w (fun (y, x) -> lines.(y).(x))
  |> Grid.map (fun _ c -> (int_of_char c - int_of_char '0'))

let trailheads grid =
  Grid.fold (fun pos n acc ->
    if n = 0 then pos::acc else acc
  ) grid []
  |> List.rev

let score_trailhead grid trailhead =
  let rec collect pos =
    Grid.fold4 (fun npos c acc ->
      if Grid.get grid pos + 1 = c then
        if c = 9
        then npos::acc
        else collect npos @ acc
      else acc
    ) grid pos []
  in collect trailhead |> List.unique |> List.length

let part1 =
  let grid = file_to_grid "input.txt" in
  List.map (score_trailhead grid) (trailheads grid)
  |> List.sum

let rate_trailheads grid trailhead =
  let rec collect pos path =
    Grid.fold4 (fun npos c acc ->
      if Grid.get grid pos + 1 = c then
        if c = 9
        then (npos::path)::acc
        else collect npos (npos :: path) @ acc
      else acc
    ) grid pos []
  in collect trailhead [trailhead] |> List.unique |> List.length

let part2 =
  let grid = file_to_grid "input.txt" in
  List.map (rate_trailheads grid) (trailheads grid)
  |> List.sum

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
