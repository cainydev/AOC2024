open Batteries

let parse_input lines =
  match lines with
  | (a::b::c::_::d::_) ->
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

let execute instr a b c pc =
  if pc >= (Array.length instr) - 1 then a, b, c, pc, None else
  let op = instr.(pc + 1) in
   let combo_op = match op with
    | 4 -> a | 5 -> b | 6 -> c
    | 7 -> failwith "Combo operand 7 appeared"
    | n -> n
  in
  match instr.(pc) with
  | 0 -> (* ADV *)
      (dp2 a combo_op), b, c, pc + 2, None
  | 1 -> (* BXL *)
      a, (b lxor op), c, pc + 2, None 
  | 2 -> (* BST *)
      a, (combo_op land 7), c, pc + 2, None
  | 3 -> (* JNZ *)
      if a = 0 then a, b, c, pc + 2, None
      else a, b, c, op, None
  | 4 -> (* BXC *)
      a, (b lxor c), c, pc + 2, None
  | 5 -> (* OUT *)
      a, b, c, pc + 2, Some (combo_op land 7)
  | 6 -> (* BDV *)
      a, (dp2 a combo_op), c, pc + 2, None
  | 7 -> (* CDV *)
      a, b, (dp2 a combo_op), pc + 2, None
  | _ -> failwith "Unknown instruction"

let compute instr a b c =
  let instr = Array.of_list instr in
  let step = execute instr in
  let rec run a b c pc =
    match step a b c pc with
    | (a, b, c, pc, None) -> if pc >= (Array.length instr) - 1 then [] else run a b c pc
    | (a, b, c, pc, Some out) -> if pc >= (Array.length instr) - 1 then [out] else out :: run a b c pc
  in run a b c 0

let part1 =
  let (a, b, c, instr) = Line_oriented.lines_of_file "input.txt" |> parse_input
  in compute instr a b c
  |> List.map (string_of_int)
  |> String.join ","

let rec num_same_suffix xs ys = match (xs, ys) with
  | (x::xs, y::ys) when x = y -> 1 + num_same_suffix xs ys
  | _ -> 0

let part2 =
  let (_, _, _, instr) = Line_oriented.lines_of_file "input.txt" |> parse_input in
  let program_len = List.length instr in
  let run a = compute instr a 0 0 in

  let rec find a tail =
    let rec try_x = function
      | 8 -> 0
      | x ->
        let new_a = (a lsl 3) lor x in
        let output = run new_a in
        let output_len = List.length output in

        if output_len < tail then try_x (x + 1)
        else if List.nth output (output_len - tail) <> List.nth instr (program_len - tail) then
          try_x (x + 1)
        else if output_len = program_len then
          new_a
        else
          let result = find new_a (tail + 1) in
          if result <> 0 then result
          else try_x (x + 1)
    in
    try_x 0
  in find 0 1

let () =
  Printf.printf "\nPart 1: %s\n" part1;
  Printf.printf "Part 2: %d\n" part2;
