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

module CoordMap = Map.Make(struct
  type t = int * int
  let compare = compare
end)

module CharMap = Map.Make(struct
  type t = char
  let compare = compare
end)

let lines_to_map lines : char CoordMap.t =
  List.mapi (fun x row ->
    List.mapi (fun y c -> ((x, y), c)) (String.to_list row)
  ) lines
  |> List.flatten
  |> List.fold_left (fun acc ((x, y), c) ->
       if c = '.' then acc else
       CoordMap.add (x, y) c acc
     ) CoordMap.empty
  
let remove_duplicates lst =
  let rec aux seen = function
    | [] -> List.rev seen
    | x :: xs ->
        if List.mem x seen then aux seen xs
        else aux (x :: seen) xs
  in
  aux [] lst

let antenna_map (lines: string list) : (int * int) list CharMap.t =
  let map : char CoordMap.t = lines_to_map lines in
  let unique_frequencies: char list =
    CoordMap.to_list map
    |> List.map (Tuple2.second)
    |> remove_duplicates
  in
  List.map (fun c -> (c,
    CoordMap.filter (fun _ v -> v = c) map
    |> CoordMap.to_list
    |> List.map Tuple2.first)
  ) unique_frequencies
  |> CharMap.of_list

let find_antinode ((x1, y1), (x2, y2)) = (x2 + (x2 - x1), y2 + (y2 - y1))

let part1 =
  let lines = read_lines "input.txt" in
  let m = lines |> antenna_map in
  let (w, h) = (String.length (List.at lines 0), List.length lines) in
  CharMap.fold (fun _ positions acc ->
    let pairs =
      List.cartesian_product positions positions
      |> List.filter (fun (p1, p2) -> p1 <> p2)
    in
    let pairs_rev =
      List.map (fun (p1, p2) -> (p2, p1)) pairs
    in
    acc @ (List.map find_antinode pairs) @ (List.map find_antinode pairs_rev) 
  ) m []
  |> remove_duplicates
  |> List.count_matching (fun (x, y) ->
      x >= 0 && x < w && y >= 0 && y < h)

let find_antinodes ((x1, y1), (x2, y2)) max_x max_y =
  let forward =
    Seq.unfold (fun (cx, cy) ->
      let new_pos = cx + (x2 - x1), cy + (y2 - y1) in
      Some(new_pos, new_pos)
    ) (x1, y1)
  in
  let backwards =
    Seq.unfold (fun (cx, cy) ->
      let new_pos = cx + (x1 - x2), cy + (y1 - y2) in
      Some(new_pos, new_pos)
    ) (x2, y2)
  in
  let in_bounds (x, y) = x >= 0 && x <= max_x && y >= 0 && y <= max_y in
  (Seq.take_while in_bounds forward |> List.of_seq)
  @ (Seq.take_while in_bounds backwards |> List.of_seq)


let part2 =
  let lines = read_lines "input.txt" in
  let m = lines |> antenna_map in
  let (w, h) = (String.length (List.at lines 0), List.length lines) in
  CharMap.fold (fun _ positions acc ->
    let pairs =
      List.cartesian_product positions positions
      |> List.filter (fun (p1, p2) -> p1 <> p2)
    in
    acc @ List.concat_map (fun p -> find_antinodes p (w-1) (h-1)) pairs
  ) m []
  |> remove_duplicates
  |> List.length 

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
