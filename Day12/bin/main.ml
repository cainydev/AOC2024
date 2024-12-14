open Batteries
open Line_oriented

module CharMap = Map.Make(Char)

let print_return m =
  CharMap.iter (Printf.printf "%c -> %i, ") m;
  print_endline ""

let file_to_grid filename =
  let lines =
    lines_of_file filename
    |> List.map (Array.of_list % String.to_list)
    |> Array.of_list
  in
  let h, w = Array.length lines, Array.length (lines.(0)) in
  Grid.init h w (fun (y, x) -> lines.(y).(x))

let to_crops_map grid =
  Grid.fold (fun p c ->
    CharMap.modify_def [] c (fun ps -> p::ps) 
  ) grid CharMap.empty

let flood_fill grid start_pos =
  let value = Grid.get grid start_pos in
  let visited = Hashtbl.create(Grid.width grid * Grid.height grid) in
  let neighbors p =
    [Grid.north p; Grid.east p; Grid.south p; Grid.west p]
  in

  let rec dfs pos =
    if not @@ Grid.inside grid pos then []
    else if Hashtbl.mem visited pos then []
    else if Grid.get grid pos <> value then []
    else (
      Hashtbl.add visited pos true;
      pos :: List.concat_map dfs (neighbors pos)
    )
  in dfs start_pos

let part1 =
  let grid = file_to_grid "test.txt" in
  Grid.fold (fun pos _crop (vs, sum) ->
    if Set.mem pos vs
    then (vs, sum)
    else
      let all_pos = flood_fill grid pos in
      let area = List.length all_pos in
      let perimeter = List.map (fun p ->
        [Grid.north p; Grid.east p; Grid.south p; Grid.west p]
        |> List.count_matching (fun n -> not @@ List.mem n all_pos)
      ) all_pos |> List.sum
      in
      (Set.add_seq (Seq.of_list all_pos) vs, sum + area * perimeter)
  ) grid (Set.empty, 0) |> Tuple2.second

let find_edges shape =
  let shape_set = Set.of_list shape in
  let is_in_shape (x, y) = Set.mem (x, y) shape_set in

  let neighbors p =
    [Grid.north p; Grid.east p; Grid.south p; Grid.west p]
  in

  let edges = List.fold_left (fun acc pos ->
    acc @ List.filter_map (fun neighbor ->
      if is_in_shape neighbor then None
      else Some (pos, neighbor)
    ) (neighbors pos)
  ) [] shape
  in
  
  let normalize x = compare x 0 in

  let dir (from_x, from_y) (to_x, to_y) =
    normalize (to_x - from_x), normalize (to_y - from_y)
  in
  
  let rec connection (from_x, from_y) to_pos (dx, dy) (clear_x, clear_y) =
    if (from_x, from_y) = to_pos then true else
    if is_in_shape (from_x + dx, from_y + dy) && (not (
      is_in_shape (from_x + dx + clear_x, from_y + dy + clear_y) 
    ))
    then connection (from_x + dx, from_y + dy) to_pos (dx, dy) (clear_x, clear_y)
    else false
  in

  let is_same_edge (pos1, neighbor1) (pos2, neighbor2) =
    let fence_dir1 = dir pos1 neighbor1 in
    let fence_dir2 = dir pos2 neighbor2 in
    let (dx, dy) = dir pos1 pos2 in
    fence_dir1 = fence_dir2
    && ((dx = 0 && abs dy = 1) || (dy = 0 && abs dx = 1))
    && connection pos1 pos2 (dx, dy) fence_dir1
  in
  
  List.fold (fun groups edge ->
    let rec insert = function
      | g::gs when is_same_edge (List.hd g) edge -> (edge::g)::gs
      | g::gs -> g::insert gs
      | [] -> [[edge]]
    in insert groups
  ) [] edges

let part2 =
  let grid = file_to_grid "input.txt" in
  Grid.fold (fun pos crop (vs, sum) ->
    if Set.mem pos vs
    then (vs, sum)
    else
      let all_pos = flood_fill grid pos in
      let area = List.length all_pos in
      let sides = find_edges all_pos |> List.length in
      (*List.iter (fun ((x1, y1), (x2, y2)) -> 
        Printf.printf "(%d, %d) -> (%d, %d)\n" x1 y1 x2 y2
      ) (find_edges all_pos);*)
      Printf.printf "%c has area %i and %i sides so fence %i\n" crop area sides (area * sides);
      (Set.add_seq (Seq.of_list all_pos) vs, sum + area * sides)
  ) grid (Set.empty, 0) |> Tuple2.second


let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
