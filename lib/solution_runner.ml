module Make (Sol : Solution.S) = struct
  let run input_file =
    let lines =
      let ic = open_in input_file in
      let rec read_lines acc =
        try
      ➜  AOC2024 git:(chore/better-project-structure) ✗ dune exec AOC2024
File "bin/main.ml", line 1, characters 5-20:
1 | open Solution_runner
         ^^^^^^^^^^^^^^^
Error: Unbound module Solution_runner
➜  AOC2024 git:(chore/better-project-structure) ✗ cat dune-project
(lang dune 3.17)

(name AOC2024)

(generate_opam_files true)

(source
 (github cainytheslave/AOC2024))

(authors "John Wagner <info@techbra.in>")

(license LICENSE)

(package
 (name AOC2024)
 (synopsis "My 2024 solutions for AoC")
 (depends ocaml dune batteries grid))
➜  AOC2024 git:(chore/better-project-structure) ✗ cat bin/dune
(executable
 (public_name AOC2024)
 (name main)
 (libraries AOC2024 days))
➜  AOC2024 git:(chore/better-project-structure) ✗ cat lib/dune
(library
 (name AOC2024)
 (modules solution solution_runner))
➜  AOC2024 git:(chore/better-project-structure) ✗ cat days/dune
(library
 (name days)
 (modules day_01
          day_02
          day_03
          day_04
          day_05
          day_06
          day_07
          day_08
          day_09)
 (libraries AOC2024 batteries grid line_oriented))    let line = input_line ic in
          read_lines (line :: acc)
        with End_of_file ->
          close_in ic;
          List.rev acc
      in
      read_lines []
    in
    let result1 = Sol.part1 lines in
    let result2 = Sol.part2 lines in
    Printf.printf "Part 1: %d\n" result1;
    Printf.printf "Part 2: %d\n" result2
end
