open Batteries

let time f x =
    let t = Sys.time() in
    let fx = f x in
    (fx, Sys.time() -. t)

let uncurry f = fun (x, y) -> f x y

let next n =
  let a = (n lxor (n lsl  6)) land 0xFFFFFF in
  let b = (a lxor (a lsr  5)) land 0xFFFFFF in
          (b lxor (b lsl 11)) land 0xFFFFFF

let diffs n =
  let rec aux n0 n0l i =
    if i <= 0 then [] else
    let n1 = next n0 in
    let n1l = n1 mod 10 in
    (n1l, n1l - n0l) :: aux n1 n1l (i - 1)
    in aux n (n mod 10) 2000

let rec zipWith4 f l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
  | (x1::xs1, x2::xs2, x3::xs3, x4::xs4) ->
      f x1 x2 x3 x4 :: zipWith4 f xs1 xs2 xs3 xs4
  | _ -> []

module SeqMap = Map.Make(struct
  type t = int * int * int * int
  let compare = compare
end)

let diffs_to_map diffs =
  let diffs1 = List.tl diffs in
  let diffs2 = List.tl diffs1 in
  let diffs3 = List.tl diffs2 in
  let zipped =
    zipWith4 (fun (_, d1) (_, d2) (_, d3) (v, d4) ->
      (d1, d2, d3, d4), v
    ) diffs diffs1 diffs2 diffs3
  in
  List.fold (fun m (k, v) ->
    if SeqMap.mem k m then m else SeqMap.add k v m
  ) SeqMap.empty zipped

let part1 () =
  Line_oriented.lines_of_file "input.txt"
  |> List.map int_of_string
  |> List.map (fun s -> Enum.fold (fun n _ -> next n) s (1--2000))
  |> List.sum

let part2 () =
  Line_oriented.lines_of_file "input.txt"
  |> List.map (diffs_to_map % diffs % int_of_string)
  |> List.reduce (SeqMap.union (fun _ a b -> Some (a + b))) 
  |> fun m -> SeqMap.fold (fun _ -> max) m 0

let () =
  Printf.printf "\nDay 22\n%!";
  (uncurry @@ Printf.printf "Part 1: %i in %fs\n") (time part1 ());
  (uncurry @@ Printf.printf "Part 2: %i in %fs\n") (time part2 ())
