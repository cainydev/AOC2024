open Batteries

let time f x =
    let t = Sys.time() in
    let fx = f x in
    (fx, Sys.time() -. t)

let uncurry f = fun (x, y) -> f x y

let part1 () = Line_oriented.lines_of_file "test.txt" |> List.length

let part2 () = Line_oriented.lines_of_file "test.txt" |> List.length

let () =
  Printf.printf "\nDay %DAY%\n%!";
  (uncurry @@ Printf.printf "Part 1: %i in %fs\n") (time part1 ());
  (uncurry @@ Printf.printf "Part 2: %i in %fs\n") (time part2 ())
