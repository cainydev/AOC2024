open Batteries

let time f x =
    let t = Sys.time() in
    let fx = f x in
    (fx, Sys.time() -. t)

let uncurry f = fun (x, y) -> f x y

type gate = AND | OR | XOR

module WireMap = Map.Make (String)

let parse_input file =
  List.partition (Fun.flip String.contains @@ ':') @@ Line_oriented.lines_of_file file
    |> fun (inits, gates) -> (
      inits |> List.map (fun i ->
        let (n, v) = String.split ~by:": " i in (String.trim n, if v = "0" then false else true)
      ) |> WireMap.of_list,
      List.tl gates |> List.map (fun s ->
        let in_gate, out = String.split ~by: " -> " s in
        match String.split_on_char ' ' in_gate with
        | [a; gate; c] -> (match gate with
            | "AND" -> (AND, a, c, out)
            | "OR"  -> ( OR, a, c, out)
            | "XOR" -> (XOR, a, c, out)
            | _ -> failwith ("Unknown gate: " ^ gate))
        | _ -> failwith ("Invalid gate format: " ^ in_gate)
      ),
      List.tl gates |> List.filter_map (fun s ->
        let _, out = String.split ~by: " -> " s in
        if String.head out 1 = "z" then Some (String.trim out) else None
      ) |> Set.of_list
    )

let step wires gates =
  List.fold (fun ws (gate, a, b, out) ->
    if WireMap.mem a ws && WireMap.mem b ws then
      let v = match gate with
        | AND -> WireMap.find a ws && WireMap.find b ws
        | OR  -> WireMap.find a ws || WireMap.find b ws
        | XOR -> WireMap.find a ws <> WireMap.find b ws
      in WireMap.add out v ws
    else ws
  ) wires gates

let run wires gates zs =
  let rec loop ws =
    if Set.for_all (fun z -> WireMap.mem z ws) zs then ws
    else loop (step ws gates)
  in
  loop wires

let of_bin_list = List.fold_lefti (fun acc i b ->
  if b = 0 then acc else b + (Int.pow 2 i)
) 0
  

let part1 () =
  let (wires, gates, zs) = parse_input "input.txt" in
  let res = run wires gates zs in
  
  Set.to_list zs
  |> List.sort String.compare
  |> List.mapi (fun i k ->
      match WireMap.find k res with
      | true -> Int.pow 2 i
      | false -> 0
  )
  |> List.sum

let to_bin_list n =
  let rec aux n =
    if n = 0 then []
    else n mod 2 :: aux (n / 2)
  and pad_right ~n ~elem xs =
    let len = List.length xs in
    if len >= n then xs
    else xs @ List.init (n - len) (const elem)
  in aux n |> pad_right ~n:64 ~elem:0

let run_with_input gates zs x y =
  let xs = to_bin_list x |> List.mapi (fun i b ->
    (Printf.sprintf "x%02d" i, if b = 0 then false else true)
  ) in
  let ys = to_bin_list y |> List.mapi (fun i b ->
    (Printf.sprintf "y%02d" i, if b = 0 then false else true)
  ) in
  let wires = WireMap.of_list (xs @ ys) in
  let res = run wires gates zs in
  Set.to_list zs
  |> List.sort String.compare
  |> List.mapi (fun i k ->
      match WireMap.find k res with
      | true -> Int.pow 2 i
      | false -> 0
  )
  |> List.sum

let test_bit_n gates zs n =
  let tests = [(0, 0, 0); (0, 1, 1); (1, 0, 1); (1, 1, 2)] in
  List.map (fun (x, y, exp) -> (x lsl n, y lsl n, exp lsl n)) tests
  |> List.fold (fun valid (x, y, expected) ->
      let res = run_with_input gates zs x y in
      if res <> expected then (
        Printf.printf "Test failed for n=%d x=%d, y=%d: expected %d, got %d\n" n x y expected res;
        false
      )
      else valid
  ) true

let find_faults gates zs: string =
  let max_z = Set.max_elt zs in
  List.fold (fun acc (gate, a, b, out) ->
    if out.[0] = 'z' && gate <> XOR && out <> max_z
    then Set.add out acc
    else
      let outS = not @@ List.mem out.[0] ['x'; 'y'; 'z']
      and aS = not @@ List.mem a.[0] ['x'; 'y'; 'z']
      and bS = not @@ List.mem b.[0] ['x'; 'y'; 'z']
      in
      if gate = XOR && outS && aS && bS
      then Set.add out acc
      else
        if gate = AND && a <> "x00" && b <> "x00"
        then List.fold (fun acc (sgate, sa, sb, _) ->
          if (out = sa || out = sb) && sgate <> OR
          then Set.add out acc else acc
        ) acc gates
        else
          if gate = XOR
          then List.fold (fun acc (sgate, sa, sb, _) ->
            if (out = sa || out = sb) && sgate = OR
            then Set.add out acc else acc
          ) acc gates
          else acc
  ) Set.empty gates
  |> Set.to_list
  |> List.sort String.compare
  |> String.concat ","

let part2 () =
  let (wires, gates, zs) = parse_input "input.txt" in
  find_faults gates zs

let () =
  Printf.printf "\nDay 24\n";
  (uncurry @@ Printf.printf "Part 1: %i in %fs\n") (time part1 ());
  (uncurry @@ Printf.printf "Part 2: %s in %fs\n") (time part2 ())
