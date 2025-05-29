open Batteries

let (#+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type pos = (int * int)
type dir = (int * int)

module State = struct
  type t = pos * dir
  let compare = compare
end

module StateAndPath = struct
  type t = State.t * (pos list)
  let compare = compare
end

module StateMap = Map.Make(State)

module StateQueue = Psq.Make(StateAndPath)(Int)

let shortest_paths
  (start: StateAndPath.t)
  (is_finished: State.t -> bool)
  (get_next: State.t -> (State.t * int) list) =

  let rec sp todo best seen dist =
    match StateQueue.pop todo with
    | None -> (best, seen |> Set.of_list |> Set.cardinal)
    | Some (((state, path), cost), rest) -> 
      if cost > StateMap.find_default max_int state dist
        then sp rest best seen dist
        else
          let newDist = StateMap.add state cost dist in
          let neighbours = get_next state |> List.map (fun (s, c) -> (s, fst s::path), cost + c) in
          let newTodo = StateQueue.add_seq (List.to_seq neighbours) rest in
          if is_finished state && cost <= best
            then sp newTodo cost (List.append seen path) newDist
            else sp newTodo best seen newDist
  in sp (StateQueue.sg start 0) max_int [] StateMap.empty

let () =
  let grid =
    Line_oriented.lines_of_file "input.txt"
    |> Array.of_list % List.map (Array.of_list % String.to_list) in

  let start_pos = Grid.find (const @@ (=) 'S') grid in
  let end_pos = Grid.find (const @@ (=) 'E') grid in
  
  let is_finished (pos, _) = pos = end_pos
  and get_next (pos, (dx, dy)) =
    [(dx, dy); (-dy, dx); (dy, -dx)]
    |> List.map (fun d ->
        if d <> (dx, dy)
        then ((pos #+ d, d), 1001)
        else ((pos #+ d, d), 1))
    |> List.filter ((<>) '#' % Grid.get grid % fst % fst) in

  let res = shortest_paths ((start_pos, (0, 1)), [start_pos]) is_finished get_next in
  (
    Printf.printf "Day 16\n";
    Printf.printf "Part 1: %i\n" (fst res);
    Printf.printf "Part 2: %i\n" (snd res);
  )
