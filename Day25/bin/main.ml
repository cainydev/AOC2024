open Batteries

let time f x =
    let t = Sys.time() in
    let fx = f x in
    (fx, Sys.time() -. t)

let uncurry f = fun (x, y) -> f x y

let parse =
  let rec aux (locks, keys) = function
  | line :: rs when String.length line = 0 -> aux (locks, keys) rs
  | l1 :: l2 :: l3 :: l4 :: l5 :: l6 :: l7 :: rs ->
      let is_lock = l1.[0] = '#' in
      let pattern =
        [l1; l2; l3; l4; l5; l6; l7]
        |> List.map (String.to_list)
        |> List.transpose
        |> (fun p -> if not is_lock then List.map (List.rev) p else p)
        |> List.map (List.fold_left (fun acc c -> if c = '#' then acc + 1 else acc) 0)
      in if is_lock then aux (pattern :: locks, keys) rs else aux (locks, pattern :: keys) rs
  | _ -> (locks, keys)
  in aux ([], [])

let part1 () =
  let (locks, keys) = Line_oriented.lines_of_file "input.txt" |> parse in
  List.cartesian_product locks keys
  |> List.filter (List.for_all (fun (x, y) -> x + y <= 7) % (uncurry List.combine))
  |> List.length

let part2 () = Line_oriented.lines_of_file "test.txt" |> List.length

let () =
  Printf.printf "\nDay 25\n%!";
  (uncurry @@ Printf.printf "Part 1: %i in %fs\n") (time part1 ());
  (uncurry @@ Printf.printf "Part 2: %i in %fs\n") (time part2 ())
