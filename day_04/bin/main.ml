open Core

let parse_ranges s =
  let l = String.split ~on:'-' s in
  match l with
  | [start; finish] -> (Int.of_string start, Int.of_string finish)
  | [] | [_ ] | _ -> (0, 0)

let is_not_overlapping fs fe ss se = fe < ss || se < fs

let calc_points filename =
  In_channel.with_file filename ~f:(fun file ->
    In_channel.fold_lines file ~init:0 ~f:(fun accum line ->
      let ranges = String.split ~on:',' line in
      match ranges with
      | [ first; second ] ->
        let (fs, fe), (ss, se) = parse_ranges first, parse_ranges second in
        if is_not_overlapping fs fe ss se then accum else accum + 1
      | [] | [_] | _  -> accum
          (* let () = printf "\n%d %d == common letter %d\n" (Set.length second_set) (Set.length first_set) value in *)
    )
  );;

let s = calc_points "input.txt"
let () = printf "\n%d\n" @@ s
