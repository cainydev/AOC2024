open Batteries

type instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

let parse_input lines =
  match lines with
  | (a::b::c::_::d::[]) ->
      let a = String.filter (Char.is_digit) a
      and b = String.filter (Char.is_digit) b
      and c = String.filter (Char.is_digit) c
      and d = String.filter (fun c -> Char.is_digit c || c = ',') d
        |> String.split_on_char ','
        |> List.map int_of_string
      in (int_of_string a, int_of_string b, int_of_string c, d)
  | _ -> failwith "Parse error"

let dp2 x p =
  if p >= 63 then 0
  else x lsr p

let compute (a, b, c, instructions) =
  let program = Array.of_list instructions in
  let last_instruction = Array.length program - 2 in
  let rec run a b c pc =
    if pc > last_instruction then [] else
    let op = program.(pc + 1) in
    let combo_op = match op with
      | 4 -> a | 5 -> b | 6 -> c
      | 7 -> failwith "Combo operand 7 appeared"
      | n -> n
    in
    match program.(pc) with
    | 0 -> (* ADV *)
        run (dp2 a combo_op) b c @@ pc + 2
    | 1 -> (* BXL *)
        run a (b lxor op) c @@ pc + 2 
    | 2 -> (* BST *)
        run a (combo_op mod 8) c @@ pc + 2
    | 3 -> (* JNZ *)
        if a = 0 then run a b c @@ pc + 2
        else run a b c op
    | 4 -> (* BXC *)
        run a (b lxor c) c @@ op + 2
    | 5 -> (* OUT *)
        (combo_op mod 8) :: (run a b c @@ op + 2)
    | 6 -> (* BDV *)
        run a (dp2 a combo_op) c @@ pc + 2
    | 7 -> (* CDV *)
        run a b (dp2 a combo_op) @@ pc + 2
    | _ -> failwith "Unknown instruction"
  in List.rev @@ run a b c 0


let part1 =
  Line_oriented.lines_of_file "test.txt"
  |> parse_input
  |> (fun (a, b, c, instr) ->
    Printf.printf "(%d, %d, %d) mit %d instructions\n" a b c (List.length instr);
    (a, b, c, instr))
  |> compute
  |> (fun s  -> 
    Printf.printf "ergab %d ergebnisse" (List.length s);s)
  |> List.map (string_of_int)
  |> String.join ","

let part2 = 0

let () =
  Printf.printf "Part 1: %s\n" part1;
  Printf.printf "Part 2: %i\n" part2
