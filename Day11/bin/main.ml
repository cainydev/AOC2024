open Batteries
open Line_oriented

module IntMap = Map.Make(BatInt)

let parse_input filename =
  let start =
    lines_of_file filename
    |> List.hd
    |> String.split_on_char ' '
    |> List.map (fun s -> int_of_string s, 1)
  in IntMap.of_list start


let reset = const 0
let incr n = (+) n

let blink map =
  IntMap.fold (fun x n m ->
    match x with
    | 0 -> m |> IntMap.modify_def 0 1 (incr n)
    | _ ->
        let s = string_of_int x in
        let l = String.length s in
        if l mod 2 = 0 then
          let first = String.sub s 0 (l / 2) |> int_of_string in
          let snd = String.sub s (l / 2) (l / 2) |> int_of_string in
          m |> IntMap.modify_def 0 first (incr n)
            |> IntMap.modify_def 0 snd (incr n)
        else m |> IntMap.modify_def 0 (x * 2024) (incr n)
  ) map (IntMap.empty)

let part1 =
  Enum.fold
    (fun stones _ -> blink stones)
    (parse_input "input.txt")
    (1 -- 25)
  |> IntMap.filterv ((<>) 0)
  |> IntMap.values
  |> Enum.sum

let part2 =
  Enum.fold
    (fun stones _ -> blink stones)
    (parse_input "input.txt")
    (1 -- 75)
  |> IntMap.filterv ((<>) 0)
  |> IntMap.values
  |> Enum.sum

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
