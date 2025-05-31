open Batteries

module State = struct
  type t = Grid.position 
  let compare = compare
end

module StateMap = Map.Make(State)
module StateQueue = Psq.Make(State)(Int)

let shortest_path
  (start: State.t)
  (is_finished: State.t -> bool)
  (get_next: State.t -> (State.t * int) list) =

  let rec sp queue dist =
    match StateQueue.pop queue with
    | None -> None  (* No path found *)
    | Some ((state, cost), rest) ->
      if cost > StateMap.find_default max_int state dist then
        sp rest dist
      else if is_finished state then
        Some cost
      else
        let dist = StateMap.add state cost dist in
        let neighbors =
          get_next state
          |> List.filter (fun (s, c) ->
              let new_cost = cost + c in
              new_cost < StateMap.find_default max_int s dist)
          |> List.map (fun (s, c) -> (s, cost + c)) in
        let queue = StateQueue.add_seq (List.to_seq neighbors) rest in
        sp queue dist
  in sp (StateQueue.sg start 0) StateMap.empty

let part1 =
  let bytes =
    Line_oriented.lines_of_file "input.txt"
    |> List.map (String.split_on_char ',')
    |> List.map (function
      | [x; y] -> (int_of_string x, int_of_string y)
      | _ -> failwith "Invalid input")
  in

  let grid = Grid.make 71 71 max_int in
  List.iteri (fun i pos -> Grid.set grid pos (min (Grid.get grid pos) i)) bytes; 
  
  let is_finished state = state = (70, 70) in
  let get_next state = Grid.fold4 (fun pos a acc ->
    if Grid.get grid pos >= 1024 then (pos, 1) :: acc else acc
  ) grid state [] in

  shortest_path (0, 0) is_finished get_next |? -1

let part2 = 
  let bytes =
    Line_oriented.lines_of_file "input.txt"
    |> List.map (String.split_on_char ',')
    |> List.map (function
      | [x; y] -> (int_of_string x, int_of_string y)
      | _ -> failwith "Invalid input")
  in
  
  let n = 71 in
  let grid = Grid.make n n max_int in
  List.iteri (fun i pos -> Grid.set grid pos (min (Grid.get grid pos) i)) bytes; 
  
  let is_finished state = state = (n - 1, n - 1) in
  let get_next n = fun state -> Grid.fold4 (fun pos a acc ->
    if Grid.get grid pos > n then (pos, 1) :: acc else acc
  ) grid state [] in

  let firstBlocking = Enum.find (fun i ->
    match shortest_path (0, 0) is_finished (get_next i) with
    | None -> true
    | Some _ -> false
  ) (0 -- (List.length bytes - 1)) in

  let (x, y) = List.nth bytes firstBlocking in
  string_of_int x ^ "," ^ string_of_int y

let () =
  Printf.printf "\n\n%!";
  Printf.printf "Part 1: %i\n%!" part1;
  Printf.printf "Part 2: %s\n%!" part2
