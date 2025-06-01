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

let part1 () =
  let (wires, gates, zs) = parse_input "input.txt" in
  let rec loop ws =
    if Set.for_all (fun z -> WireMap.mem z ws) zs then ws
    else loop (step ws gates)
  in
  let res = loop wires in
  
  Set.to_list zs
  |> List.sort String.compare
  |> List.mapi (fun i k ->
      match WireMap.find k res with
      | true -> Int.pow 2 i
      | false -> 0
  )
  |> List.sum

let part2 () = Line_oriented.lines_of_file "test.txt" |> List.length

let () =
  Printf.printf "\nDay 24\n%!";
  (uncurry @@ Printf.printf "Part 1: %i in %fs\n") (time part1 ());
  (uncurry @@ Printf.printf "Part 2: %i in %fs\n") (time part2 ())
