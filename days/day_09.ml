open Batteries
open Line_oriented

let seq_to_memory seq =
  let block_id = ref 0 in
  List.mapi (fun i x ->
      if i mod 2 = 0
      then List.init x (Fun.const (Some !block_id))
      else begin
        block_id := !block_id + 1;
        List.init x (Fun.const None)
      end 
  ) seq |> List.concat |> Array.of_list

let checksum mem =
  let pos = ref 0 in
  let pos_r = ref (Array.length mem - 1) in
  let sum = ref 0 in
  while !pos_r >= !pos do
    while mem.(!pos_r) = None do pos_r :=  !pos_r - 1; done;
    
    (match mem.(!pos) with
    | Some n -> sum := !sum + (!pos * n)
    | None -> begin
      sum := !sum + (!pos * Option.get mem.(!pos_r));
      pos_r := !pos_r - 1
    end);

    pos := !pos + 1
  done;
  !sum

let part1 =
  lines_of_file "input.txt"
  |> List.hd
  |> String.to_list
  |> List.map (fun c -> int_of_char c - int_of_char '0')
  |> seq_to_memory
  |> checksum

let checksum_2 mem =
  let c_mem = ref (Array.copy mem) in
  let c_pos = ref (Array.length !c_mem - 1) in

  while !c_pos > 0 do
    while !c_pos > 0 && mem.(!c_pos) = None do
      c_pos := !c_pos - 1
    done;
    
    let filename = Option.get (!c_mem.(!c_pos)) in
    let length = ref 0 in
    let i = ref (!c_pos) in

    while !i >= 0 && !c_mem.(!i) = Some filename do
      length := !length + 1;
      i := !i - 1
    done;

    let length_of_space = ref 0 in
    let j = ref 0 in

    while !length_of_space < !length && !j <= (!c_pos - !length) do
      (match !c_mem.(!j) with
      | Some _ -> length_of_space := 0
      | None -> length_of_space := !length_of_space + 1);

      j := !j + 1
    done;
    
    if !length_of_space = !length then begin
      let start_new = !j - !length in
      for k = 0 to !length - 1 do
        !c_mem.(start_new + k) <- Some filename;
        !c_mem.(!c_pos - k) <- None
      done
    end;

    c_pos := !c_pos - !length
  done;
      
  Array.fold_lefti (fun acc index -> function
    | Some elem -> acc + index * elem
    | None -> acc) 0 !c_mem

let part2 = 
  lines_of_file "input.txt"
  |> List.hd
  |> String.to_list
  |> List.map (fun c -> int_of_char c - int_of_char '0')
  |> seq_to_memory
  |> checksum_2

let () =
  Printf.printf "Part 1: %i\n" part1;
  Printf.printf "Part 2: %i\n" part2
