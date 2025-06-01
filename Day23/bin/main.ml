open Batteries

let time f x =
    let t = Sys.time() in
    let fx = f x in
    (fx, Sys.time() -. t)

let uncurry f = fun (x, y) -> f x y

module M = Map.Make (String)

let rec combine2 = function
  | [] -> []
  | x :: xs ->
      let pairs = List.map (fun y -> (x, y)) xs in
      pairs @ combine2 xs

let part1 () =
  let cons =
    Line_oriented.lines_of_file "input.txt"
    |> List.concat_map (fun s -> let l, r = String.split ~by:"-" s in [l, r; r, l])
    |> List.fold_left (fun m (l, r) ->
        M.update_stdlib l (function
            | None -> Some [r]
            | Some lst -> Some (r :: lst)
        ) m
      ) M.empty
  in

  M.fold (fun k vs s ->
    List.fold (fun s (k2, k3) ->
      if List.exists ((=) k3) (M.find k2 cons) then
        if String.head k 1 = "t" || String.head k2 1 = "t" || String.head k3 1 = "t"
        then Set.add (List.sort String.compare [k; k2; k3]) s
        else s
      else s
    ) s (combine2 vs)
  ) cons Set.empty
  |> Set.cardinal
 
(* https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm *)
let bron_kerbosch (type a) (neighbors: a -> a BatSet.t) (nodes: a BatSet.t): a BatSet.t list =
  let rec aux r p x =
    if Set.is_empty p && Set.is_empty x then [r]
    else Set.fold (fun v (p, x, cliques) ->
      let ns = neighbors v in
      let new_cliques = aux (Set.add v r) (Set.intersect p ns) (Set.intersect x ns) in
      (Set.remove v p, Set.add v x, cliques @ new_cliques)
    ) p (p, x, []) |> Tuple3.third
  in aux Set.empty nodes Set.empty

let part2 () =
  let cons =
    Line_oriented.lines_of_file "input.txt"
    |> List.concat_map (fun s -> let l, r = String.split ~by:"-" s in [l, r; r, l])
    |> List.fold_left (fun m (l, r) ->
        M.update_stdlib l (function
            | None -> Some (Set.singleton r)
            | Some ns -> Some (Set.add r ns)
        ) m
      ) M.empty
  in

  bron_kerbosch (fun v -> M.find v cons) (M.keys cons |> Set.of_enum) 
  |> List.max ~cmp:(fun a b -> Set.cardinal a - Set.cardinal b)
  |> Set.to_list
  |> List.sort String.compare
  |> String.concat ","

let () =
  Printf.printf "\nDay 23\n";
  (uncurry @@ Printf.printf "Part 1: %i in %fs\n") (time part1 ());
  (uncurry @@ Printf.printf "Part 2: %s in %fs\n") (time part2 ())
