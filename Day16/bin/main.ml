open Batteries

let (#+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type pos = (int * int)
type dir = (int * int)

module State = struct
  type t = pos * dir
  let compare = compare
end

(* (f, g, h) *)
module Priority = struct
  type t = int * int * int
  let compare = compare
end

module StateMap = Map.Make(State)

module StateQueue = Psq.Make(State)(Priority)

let shortest_path
  (start: State.t)
  (is_finished: State.t -> bool)
  (get_next: State.t -> (State.t * int) list)
  (heuristic: State.t -> int) =

  let initial_h = heuristic start in

  let rec reconstruct_path came_from current =
    match StateMap.find_opt current came_from with
    | None -> [fst current]
    | Some previous -> reconstruct_path came_from previous @ [fst current]
  in

  let rec sp open_set closed came_from =
    match StateQueue.pop open_set with
    | None -> None
    | Some ((state, (_f, g, _h)), rest_open) ->
      if is_finished state then
        let path = reconstruct_path came_from state in
        Some (path, g) (* Return path and cost *)
      else (
        let neighbors =
          get_next state
          |> List.filter (fun (new_state, _cost) ->
            not @@ StateMap.mem new_state closed)
        in
        let new_open, new_came_from =
          List.fold_left
            (fun (pq, cf) (new_state, cost) ->
              let g_new = g + cost in
              let h_new = heuristic new_state in
              let f_new = g_new + h_new in
              let updated_pq =
                StateQueue.update new_state (function
                  | None -> Some (f_new, g_new, h_new)
                  | Some (old_f, old_g, old_h) ->
                    if g_new < old_g then Some (f_new, g_new, h_new) else Some (old_f, old_g, old_h)
                ) pq
              in
              let updated_cf =
                if not (StateMap.mem new_state closed) then
                  StateMap.add new_state state cf
                else cf
              in
              (updated_pq, updated_cf))
            (rest_open, came_from) neighbors
        in
        sp new_open (StateMap.add state g closed) new_came_from
      )

  in sp (StateQueue.sg start (initial_h, 0, initial_h)) StateMap.empty StateMap.empty

let part1 =
  let grid =
    Line_oriented.lines_of_file "input.txt"
    |> Array.of_list % List.map (Array.of_list % String.to_list)
  in
  let start_pos = Grid.find (const @@ (=) 'S') grid in
  let end_pos = Grid.find (const @@ (=) 'E') grid in
  
  let is_finished (pos, _) = pos = end_pos
  and get_next (pos, (dx, dy)) =
    [(dx, dy); (-dy, dx); (dy, -dx)]
    |> List.map (fun d ->
        if d <> (dx, dy)
        then ((pos #+ d, d), 1001)
        else ((pos #+ d, d), 1))
    |> List.filter ((<>) '#' % Grid.get grid % fst % fst)
  and heuristic ((x, y), _) =
    let (ex, ey) = end_pos in (abs ex - x + abs ey - y)
  in

  match shortest_path (start_pos, (0, 1)) is_finished get_next heuristic with
  | None -> failwith "Couldnt find a path"
  | Some (_, score) -> score


module StateSet = Set.Make(State)

module StateWithHistory = struct
  type t = State.t * StateSet.t
  let compare (s, _) (s2, _) = compare s s2
end

module StateWithHistoryQueue = Psq.Make(StateWithHistory)(Priority)

let shortest_path2
  (start: State.t)
  (is_finished: StateWithHistory.t -> bool)
  (get_next: StateWithHistory.t -> (StateWithHistory.t * int) list)
  (heuristic: StateWithHistory.t -> int)
  (max_cost: int) : StateSet.t =

  let initial_h = heuristic (start, StateSet.empty) in

  let rec sp open_set closed visited =
    match StateQueue.pop open_set with
    | None -> None
    | Some (((state, history), (_f, g, _h)), rest_open) ->
      if g > max_cost then visited
      if is_finished (state)
      else (
        let neighbors =
          get_next (state, history)
          |> List.filter (fun ((new_state, new_history), _cost) ->
            not @@ StateMap.mem new_state closed)
        in
        let new_open, new_visited =
          List.fold_left
            (fun (pq, cf) (new_state, cost) ->
              let g_new = g + cost in
              let h_new = heuristic new_state in
              let f_new = g_new + h_new in
              let updated_pq =
                StateQueue.update new_state (function
                  | None -> Some (f_new, g_new, h_new)
                  | Some (old_f, old_g, old_h) ->
                    if g_new < old_g then Some (f_new, g_new, h_new) else Some (old_f, old_g, old_h)
                ) pq
              in
              let updated_cf =
                if not (StateMap.mem new_state closed) then
                  StateMap.add new_state state cf
                else cf
              in
              (updated_pq, updated_cf))
            (rest_open, came_from) neighbors
        in
        sp new_open (StateMap.add state g closed) new_came_from
      )

  in sp (StateWithHistoryQueue.sg (start, StateSet.empty) (initial_h, 0, initial_h)) StateMap.empty StateSet.empty

let part2 = 
  let grid =
    Line_oriented.lines_of_file "input.txt"
    |> Array.of_list % List.map (Array.of_list % String.to_list)
  in
  let start_pos = Grid.find (const @@ (=) 'S') grid in
  let end_pos = Grid.find (const @@ (=) 'E') grid in
  
  let is_finished (pos, _) = pos = end_pos
  and get_next g (pos, (dx, dy)) =
    [(dx, dy); (-dy, dx); (dy, -dx)]
    |> List.map (fun d ->
        if d <> (dx, dy)
        then ((pos #+ d, d), 1001)
        else ((pos #+ d, d), 1))
    |> List.filter ((<>) '#' % Grid.get g % fst % fst)
  and heuristic ((x, y), _) =
    let (ex, ey) = end_pos in (abs ex - x + abs ey - y)
  in
  let (_, cost) = Option.get @@
    shortest_path (start_pos, (0, 1)) is_finished (get_next grid) heuristic
  in
  let is_finished2 ((pos, _), _) = pos = end_pos
  and get_next2 g ((pos, (dx, dy)), history) =
    [(dx, dy); (-dy, dx); (dy, -dx)]
    |> List.map (fun d ->
        if d <> (dx, dy)
        then (((pos #+ d, d), StateSet.add (pos #+ d, d) history), 1001)
        else (((pos #+ d, d), StateSet.add (pos #+ d, d) history), 1)
    )
    |> List.filter (fun (((pos, dir), history), _) ->
        Grid.get g pos <> '#' && not (StateSet.mem (pos, dir) history)
    )
  and heuristic2 (((x, y), _), _) =
    let (ex, ey) = end_pos in (abs ex - x + abs ey - y)
  in
  
  StateSet.cardinal @@
    shortest_path2 (start_pos, (0, 1)) is_finished2 (get_next2 grid) heuristic2 cost 
  

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2;
