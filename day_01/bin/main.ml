open Core
open Stdio

let sum_file filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:([], 0) ~f:(fun (elf_calories, curr_calories) line ->
      match line with
        | "" -> (curr_calories :: elf_calories, 0)
        | _ -> (elf_calories, curr_calories + Int.of_string line)
      ))

let (elf_calories, _) = sum_file "input.txt"
let sorted_elf_calories = List.rev @@ List.sort elf_calories ~compare:Int.compare
let ans = match sorted_elf_calories with
      | []      -> 0
      | first :: [] -> first
      | first :: second :: [] -> first + second
      | first :: second :: third :: _ -> first + second + third

let () = printf "%d" ans
