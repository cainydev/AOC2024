open Batteries

let (#-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let (#+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

module Keypad = Hashtbl.Make(Char)

let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx

let numpad = Keypad.of_list [
  '7', (0, 0); '8', (1, 0); '9', (2, 0);
  '4', (0, 1); '5', (1, 1); '6', (2, 1);
  '1', (0, 2); '2', (1, 2); '3', (2, 2);
  ' ', (0, 3); '0', (1, 3); 'A', (2, 3);
]

let dirpad = Keypad.of_list [
  ' ', (0, 0); '^', (1, 0); 'A', (2, 0);
  '<', (0, 1); 'v', (1, 1); '>', (2, 1);
]

let pack (a,b,c,d,e,f) = Int64. 
    (logor (shift_left (of_int a) 48)
    (logor (shift_left (of_int b) 40)
    (logor (shift_left (of_int c) 32)
    (logor (shift_left (of_int d) 24)
    (logor (shift_left (of_int e) 16)
    (shift_left (of_int f) 8))))))

let press_table = Hashtbl.create 1000
let press (cx, cy) (tx, ty) (fx, fy) : string =
  let key = pack (cx, cy, tx, ty, fx, fy) in
  match Hashtbl.find_opt press_table key with
  | Some res -> res
  | None ->
    let (x, y) = (tx - cx, ty - cy) in
    let buf = Buffer.create 10 in

    let add dir count =
      for _ = 1 to count do Buffer.add_char buf dir done
    in

    if x = 0 || y = 0 then (
      add '>' (max 0 x);
      add '^' (max 0 (-y));
      add 'v' (max 0 y);
      add '<' (max 0 (-x));
    ) else (
      match x, y with
      | x, y when x < 0 && y < 0 ->
          if fy = cy && fx < cx && fx >= tx then (
            add '^' (-y); add '<' (-x)
          ) else (
            add '<' (-x); add '^' (-y)
          )
      | x, y when x < 0 && y > 0 ->
          if fy = cy && fx < cx && fx >= tx then (
            add 'v' y; add '<' (-x)
          ) else (
            add '<' (-x); add 'v' y
          )
      | x, y when x > 0 && y > 0 ->
          if fx = cx && fy > cy && fy <= ty then (
            add '>' x; add 'v' y
          ) else (
            add 'v' y; add '>' x
          )
      | x, y when x > 0 && y < 0 ->
          if fx = cx && fy < cy && fy >= ty then (
            add '>' x; add '^' (-y)
          ) else (
            add '^' (-y); add '>' x
          )
      | _ -> failwith "Unexpected direction"
    );

    Buffer.add_char buf 'A';
    let res = Buffer.contents buf in
    Hashtbl.add press_table key res;
    res

let seq_table = Hashtbl.create 100_000
let rec gen_seq keypad pos code i len buf =
  if i >= len then ()
  else
    let c = code.[i] in
    let target = Keypad.find keypad c in
    let forbidden = Keypad.find keypad ' ' in
    let key = (c, pos) in
    let step =
      match Hashtbl.find_opt seq_table key with
      | Some cached -> cached
      | None ->
          let res = press pos target forbidden in
          Hashtbl.add seq_table key res;
          res
    in
    Buffer.add_string buf step;
    gen_seq keypad target code (i + 1) len buf

let gen_seq_main keypad code =
  let buf = Buffer.create 10000 in
  let start_pos = Keypad.find keypad 'A' in
  gen_seq keypad start_pos code 0 (String.length code) buf;
  Buffer.contents buf

let length depth code =
  let length_table = Hashtbl.create 1000 in
  let rec aux depth code =
    if Hashtbl.mem length_table (depth, code) then
      Hashtbl.find length_table (depth, code)
    else
      let res = match depth with
      | 0 -> String.length code
      | _ -> String.fold_lefti (fun acc i c ->
          let start = if i = 0 then Keypad.find dirpad 'A' else Keypad.find dirpad (code.[i - 1]) in
          acc + aux (depth - 1) (press start (Keypad.find dirpad c) (Keypad.find dirpad ' '))
      ) 0 code
      in Hashtbl.add length_table (depth, code) res; res
  in aux depth code

let part1 () =
  let codes = Line_oriented.lines_of_file "input.txt"
    |> List.map String.trim in
  
  codes
    |> List.map (fun c ->
        let number = int_of_string @@ String.filter Char.is_digit c in
        let length = gen_seq_main numpad c |> length 2 in
        length * number
    )
    |> List.sum


let part2 () =
  let codes = Line_oriented.lines_of_file "input.txt"
    |> List.map String.trim in
    
    codes
    |> List.map (fun c ->
        let number = int_of_string @@ String.filter Char.is_digit c in
        let length = gen_seq_main numpad c |> length 25 in
        length * number
    )
    |> List.sum

let () =
  Printf.printf "\nDay 21\n%!";
  Printf.printf "Part 1: %i\n%!" (time part1 ());
  Printf.printf "Part 2: %i\n%!" (time part2 ())
