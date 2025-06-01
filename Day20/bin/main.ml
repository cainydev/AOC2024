open Batteries

let (#+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let (#-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

module State = struct
  type t = Grid.position * (Grid.position option)
  let compare = compare 
end

module StateMap = Map.Make(State)
module StateQueue = Psq.Make(State)(Int)

let a_star
  (start: State.t)
  (is_end: State.t -> bool)
  (get_next: State.t -> (State.t * int) list)
  (heuristic: State.t -> int): (State.t * (int StateMap.t) * (State.t list)) option =
  
  let reconstruct_path came_from current =
    let rec aux current =
      match StateMap.find_opt current came_from with
      | None -> [current]
      | Some state -> current :: aux state
    in aux current |> List.rev
  in

  let rec aux open_set came_from g_score =
    match StateQueue.pop open_set with
    | None -> None
    | Some ((state, f), open_set) ->
        if is_end state then Some (state, g_score, reconstruct_path came_from state)
        else
          let (open_set, came_from, g_score) =
            List.fold (fun (open_set, came_from, g_score) (neighbor, cost) ->
            let tentative_g = StateMap.find_default max_int state g_score + cost in
            if tentative_g < StateMap.find_default max_int neighbor g_score then
              let open_set = StateQueue.update neighbor (fun _ ->
                Some (tentative_g + heuristic neighbor)
              ) open_set
              and came_from = StateMap.add neighbor state came_from
              and g_score = StateMap.add neighbor tentative_g g_score
              in (open_set, came_from, g_score)
            else (open_set, came_from, g_score)
          ) (open_set, came_from, g_score) (get_next state)
          in
          aux open_set came_from g_score
  in aux
    (StateQueue.sg start (heuristic start))
    (StateMap.empty)
    (StateMap.singleton start 0)

let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let part1 =
  let grid =
    Line_oriented.lines_of_file "input.txt"
    |> Array.of_list % List.map (Array.of_list % String.to_list)
  in

  let start_pos = Grid.find (const @@ (=) 'S') grid in
  let end_pos = Grid.find (const @@ (=) 'E') grid in
  let is_end (pos, _) = pos = end_pos
  and get_next forbidden_cheats = fun (pos, cheat) ->
    match cheat with
    | Some _ -> Grid.fold4 (fun n_pos c ns ->
        if c <> '#' then ((n_pos, cheat), 1) :: ns else ns
      ) grid pos []
    | None ->  Grid.fold4 (fun n_pos c ns ->
        if c <> '#' then ((n_pos, cheat), 1) :: ns
        else
          if Set.mem n_pos forbidden_cheats then ns else
          let dir = n_pos #- pos in
          let cheat_target = pos #+ dir #+ dir in
          if Grid.inside grid cheat_target && Grid.get grid cheat_target <> '#' then
            ((cheat_target, Some n_pos), 2) :: ns
          else ns
      ) grid pos []
  and heuristic (pos, _) = manhattan pos end_pos in
  
  let g_score = match a_star (start_pos, Some (0, 0)) is_end (get_next Set.empty) heuristic with
  | None -> failwith "Couldn't find baseline route"
  | Some (_, g_score, _) -> g_score
  in
  
  StateMap.fold (fun state cost acc ->
    let pos, cheat = state in
    acc + List.fold (fun acc dir ->
      if not (Grid.inside grid (pos |> dir |> dir)) then acc
      else
        match StateMap.find_opt (pos |> dir |> dir, cheat) g_score with
        | None -> acc
        | Some c -> if c - cost - 2 >= 100 then acc + 1 else acc
    ) 0 [Grid.north; Grid.east; Grid.south; Grid.west] 
  ) g_score 0 

let part2 =
  let grid =
    Line_oriented.lines_of_file "input.txt"
    |> Array.of_list % List.map (Array.of_list % String.to_list)
  in

  let start_pos = Grid.find (const @@ (=) 'S') grid in
  let end_pos = Grid.find (const @@ (=) 'E') grid in
  let is_end (pos, _) = pos = end_pos
  and get_next forbidden_cheats = fun (pos, cheat) ->
    match cheat with
    | Some _ -> Grid.fold4 (fun n_pos c ns ->
        if c <> '#' then ((n_pos, cheat), 1) :: ns else ns
      ) grid pos []
    | None ->  Grid.fold4 (fun n_pos c ns ->
        if c <> '#' then ((n_pos, cheat), 1) :: ns
        else
          if Set.mem n_pos forbidden_cheats then ns else
          let dir = n_pos #- pos in
          let cheat_target = pos #+ dir #+ dir in
          if Grid.inside grid cheat_target && Grid.get grid cheat_target <> '#' then
            ((cheat_target, Some n_pos), 2) :: ns
          else ns
      ) grid pos []
  and heuristic (pos, _) = manhattan pos end_pos in
  
  let g_score = match a_star (start_pos, Some (0, 0)) is_end (get_next Set.empty) heuristic with
  | None -> failwith "Couldn't find baseline route"
  | Some (_, g_score, _) -> g_score
  in
  
  StateMap.fold (fun state cost acc ->
    let p1, cheat = state in
    StateMap.filter (fun (p2, _) c ->
      let d = manhattan p1 p2
      in d <= 20 && c - cost - d >= 100
    ) g_score
    |> StateMap.cardinal
    |> (+) acc
  ) g_score 0 

let () =
  Printf.printf "\nPart 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
